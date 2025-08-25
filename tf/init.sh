#!/usr/bin/env bash
set -euo pipefail

apt-get update
apt-get upgrade -q -y

apt-get install -q -y nginx certbot python3-certbot-nginx logrotate
snap install aws-cli --classic

## Disable IP website

cat <<'END_DISABLE_IP' > /etc/nginx/sites-available/default
server {
  listen 80 default_server;
  listen [::]:80 default_server;
  listen 443 ssl default_server;
  listen [::]:443 ssl default_server;
  ssl_reject_handshake on;
  server_name _;
  return 444;
}
END_DISABLE_IP

DOMAIN=cuddly-octo-palm-tree.com
mkdir -p /var/www/$DOMAIN/html

cat <<CONFIG > /etc/nginx/sites-available/$DOMAIN
server {
  listen 80;
  listen [::]:80;

  root /var/www/$DOMAIN/html;
  index index.html;

  server_name $DOMAIN www.$DOMAIN;

  location / {
    try_files \$uri \$uri/ =404;
  }
}
CONFIG

ln -s /etc/nginx/sites-available/$DOMAIN /etc/nginx/sites-enabled/

S3=s3://cuddly-octo-palm-tree
# This is a Terraform substitution!
TARBALL=public/${version}.tar.gz
if aws s3 ls $S3/$TARBALL; then
    aws s3 cp $S3/$TARBALL /tmp/blog
    tar xzf /tmp/blog -C /var/www/$DOMAIN/html/
    echo "${version}" > /var/www/$DOMAIN/html/version.txt
else
    cat <<'INDEX' > /var/www/$DOMAIN/html/index.html
<html>
<body>
Website not initialized yet.
</body>
</html>
INDEX
fi

chown -R ubuntu:ubuntu /var/www/$DOMAIN/html
systemctl restart nginx

CERT=cert/current.tar.gz
if aws s3 ls $S3/$CERT; then
    aws s3 cp $S3/$CERT /tmp/cert
    tar xzf /tmp/cert -C /etc/letsencrypt
fi

do_certs() {
    certbot run --nginx \
                --reinstall \
                --non-interactive \
                --agree-tos \
                --email gary.verhaegen@gmail.com \
                --redirect \
                -d $DOMAIN \
                -d www.$DOMAIN
}

do_certs

cd /etc/letsencrypt
tar czf /tmp/new-cert .
aws s3 cp /tmp/new-cert $S3/cert/current.tar.gz
TOKEN=$(curl -X PUT "http://169.254.169.254/latest/api/token" -H "X-aws-ec2-metadata-token-ttl-seconds: 21600")
INSTANCE_ID=$(curl -s -H "X-aws-ec2-metadata-token: $TOKEN" http://169.254.169.254/latest/meta-data/instance-id)
aws s3 cp /tmp/new-cert $S3/cert/$(date -u +'%Y-%m-%dT%H-%M-%S')-$INSTANCE_ID.tar.gz

cat <<LOGROTATE > /etc/logrotate.d/nginx
/var/log/nginx/*.log {
  rotate 14
  create 0640 www-data adm
  compress
  daily
  dateext
  dateformat -%Y-%m-%d-%H-%M-%S-$INSTANCE_ID
  nomail
  missingok
  sharedscripts
  prerotate
    if [ -d /etc/logrotate.d/httpd-prerotate ]; then
      run-parts /etc/logrotate.d/httpd-prerotate
    fi
  endscript
  postrotate
    invoke-rc.d nginx rotate >/dev/null 2>&1
  endscript
  lastaction
    cd /var/log/nginx
    for f in *.gz; do
      aws s3 cp \$f $S3/logs/\$f
    done
  endscript
}
LOGROTATE

SERVICE=/etc/systemd/system/save_logs
cat <<SAVE_LOGS > $SERVICE
#!/usr/bin/env bash

set -euo pipefail

cd /var/log/nginx
for f in access.log error.log; do
  gzip -9 \$f
  aws s3 cp \$f.gz s3://cuddly-octo-palm-tree/logs/\$f-\$(date +%Y-%m-%d-%H-%M-%S)-$INSTANCE_ID-shutdown.gz
done
SAVE_LOGS
chmod +x $SERVICE

cat <<SERVICE_DEFINITION > $SERVICE.service
[Unit]
Description=
Requires=network-online.target
After=network-online.target

[Service]
Type=oneshot
ExecStop=$SERVICE
RemainAfterExit=true

[Install]
WantedBy=multi-user.target
SERVICE_DEFINITION
systemctl enable $SERVICE.service
systemctl start save_logs

sleep 3600

for d in $DOMAIN www.$DOMAIN; do
    sed -i /etc/letsencrypt/renewal/$d -e 's/.*renew_before.*/renew_before_expiry = 40 days/'
done

do_certs
