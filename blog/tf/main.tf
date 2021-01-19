terraform {
  required_providers {
    aws = {
      source  = "hashicorp/aws"
      version = "~> 3.0"
    }
  }
}

provider "aws" {
  region = "us-east-1"
}

variable "email" {
  type = string
}

variable "domain" {
  type = string
}

variable "public_ssh_key" {
  type = string
}

data "aws_ami" "ubuntu" {
  most_recent = true

  filter {
    name   = "name"
    values = ["ubuntu/images/hvm-ssd/ubuntu-focal-20.04-amd64-server-*"]
  }

  filter {
    name   = "virtualization-type"
    values = ["hvm"]
  }

  owners = ["099720109477"] # Canonical
}

resource "aws_vpc" "main" {
  cidr_block = "10.0.0.0/16"
}

resource "aws_internet_gateway" "gw" {
  vpc_id = aws_vpc.main.id
}

resource "aws_route_table" "open" {
  vpc_id = aws_vpc.main.id

  route {
    cidr_block = "0.0.0.0/0"
    gateway_id = aws_internet_gateway.gw.id
  }

}

resource "aws_subnet" "open" {
  vpc_id     = aws_vpc.main.id
  cidr_block = "10.0.0.0/24"
}

resource "aws_route_table_association" "a" {
  subnet_id      = aws_subnet.open.id
  route_table_id = aws_route_table.open.id
}

resource "aws_security_group" "allow_http" {
  name   = "allow_http"
  vpc_id = aws_vpc.main.id

  ingress {
    from_port   = 80
    to_port     = 80
    protocol    = "tcp"
    cidr_blocks = ["0.0.0.0/0"]
  }

  ingress {
    from_port   = 443
    to_port     = 443
    protocol    = "tcp"
    cidr_blocks = ["0.0.0.0/0"]
  }
}

resource "aws_security_group" "allow_ssh" {
  name   = "allow_ssh"
  vpc_id = aws_vpc.main.id

  ingress {
    from_port   = 22
    to_port     = 22
    protocol    = "tcp"
    cidr_blocks = ["0.0.0.0/0"]
  }
}

resource "aws_instance" "web" {
  ami           = data.aws_ami.ubuntu.id
  instance_type = "t3.nano"
  subnet_id     = aws_subnet.open.id

  associate_public_ip_address = true

  root_block_device {
    volume_size = 20
  }

  vpc_security_group_ids = [
    aws_security_group.allow_http.id,
    #aws_security_group.allow_ssh.id,
  ]

  user_data = <<EOF
#!/usr/bin/env bash
set -euo pipefail

mkdir -p /home/ubuntu/.ssh
echo "${var.public_ssh_key}" > /home/ubuntu/.ssh/authorized_users
chown -R ubuntu /home/ubuntu
chmod 0400 /home/ubuntu/.ssh/authorized_keys

apt-get update
apt-get upgrade -q -y

apt-get install -q -y nginx certbot python3-certbot-nginx

DOMAIN=${var.domain}
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

cat <<'BLOG' > /var/www/$DOMAIN/html/index.html
<pre>
# Turning on die-on-error in Bash scripts

Bash is a great programming environment, in the same sense that JavaScript is a
great programming language. That is, it's a pretty terrible design by any
reasonable modern standard, but it's got reach. When it comes to automating
servers, there are still many tasks for which Bash is hard to beat.

Bash does not support modern concepts like exceptions, but it does have _some_
facilities for managing errors. The most important one is probably to not
silently ignore errors. It's great that Bash supports that; it's unfortunate
that it's turned off by default. In most installations, the following runs to
completion:

```shell
$ cat script.sh
does-not-exist
echo "hello"
$ bash script.sh
script.sh: line 1: does-not-exist: command not found
hello
$ echo $?
0
```

Not only did execution not stop on the error, but the script has overall
signalled successful completion. This is in most cases a huge deal: Bash
generally deals with killing processes and deleting files, so you absolutely do
not want to keep going in the presence of unexpected errors. This doesn't
really seem like a big deal in this silly example, but what about the
following?

```bash
do-backup
send-email "admin@example.org" "Backup completed, you can sleep soundly."
```

Believing you have backups when you actually don't can be a Very Bad Thing™.

There is a simple fix for this issue. It's far from covering all of the Bash
safety issues, but it does go a long way: the `-e` flag. Behold:

```shell
$ cat script.sh
does-not-exist
echo "hello"
$ bash -e script.sh
script.sh: line 1: does-not-exist: command not found
$ echo $?
127
$
```

This is a lot better. There is still one small wrinkle, though: you have to
remember to put that `-e` on your Bash invocation every single time. Moreover,
if you share that script with someone else, they also need to know to set that
flag.

Fortunately, Bash has a solution for this too: you can set such flags at any
point from the Bash script itself, and it will turn the flag on from that point
on. So this should really be the first thing you do in every Bash script.

```shell
$ cat script.sh
set -e
does-not-exist
echo "hello"
$ bash script.sh
script.sh: line 1: does-not-exist: command not found
$ echo $?
127
$
```
</pre>
BLOG

chown -R ubuntu:ubuntu /var/www/$DOMAIN/html
systemctl restart nginx
certbot run --nginx \
            --non-interactive \
            --agree-tos \
            --email ${var.email} \
            --redirect \
            -d $DOMAIN \
            -d www.$DOMAIN

EOF

  depends_on = [aws_internet_gateway.gw]

}

resource "aws_eip" "ip" {
  instance   = aws_instance.web.id
  depends_on = [aws_internet_gateway.gw]
}
