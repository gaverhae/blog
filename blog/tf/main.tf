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
    from_port   = 22
    to_port     = 22
    protocol    = "tcp"
    cidr_blocks = ["0.0.0.0/0"]
  }

  egress {
    from_port   = 0
    to_port     = 0
    protocol    = "-1"
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

  vpc_security_group_ids = [aws_security_group.allow_http.id]

  user_data = <<EOF
#!/usr/bin/env bash
set -euo pipefail

mkdir -p /home/ubuntu/.ssh
echo "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQCwYQn97wq60S+Wyvdp3BVRtN5uAcaoJeWbpkeDPEa4A9qauhKOSJJ3UuO+Yqrh7wC8sUJGl0gIuilZbmZK3NmLFc6fla99/Di6rrz6DScat0IpSQZV51NWRxpxqgc0BYQqtrlt1xakT1wWNbbv3CWHeZetvH5rB+sBBGlGtXJhVRExRqWyIvDwVaL9cvGt/tyBnUjIwDBHV1lD4ee2epjLpQrCGHoEq17k8TFsA6NE8Obk1hefabelwcRTDdC4N09OYDh4ogmavp0986H/SUeN1Dk+68+XMeWd4jQ7a9gqnGuNrM4ENnLthBDZL6/CM69oLmGpZhVuduLuQ8nneB1r gverhaegen@ALMB0115.local" > /home/ubuntu/.ssh/authorized_users
chown -R ubuntu /home/ubuntu
chmod 0400 /home/ubuntu/.ssh/authorized_keys

apt-get update
apt-get upgrade -q -y

apt-get install -q -y nginx

cat <<'BLOG' > /var/www/html/index.html
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
generally deals with killing processes and deleting files, you absolutely do
not want to keep going in the presence of unexpected errors. This doesn't
really seem like a big deal in this silly example, but what about the
following?

```bash
do-backup
send-email "admin@example.org" "Backup completed, you can sleep soundly."
```

Believing you have backups when you actually don't can be a Very Bad Thing™.

There is a simple fix for this issue. It's for from covering all of the Bash
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
point from the Bash script itself, and it sill turn the flag on from that point
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

EOF

  depends_on = [aws_internet_gateway.gw]

}
