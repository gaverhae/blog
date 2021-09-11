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

variable "deployed_json" {
  type = string
}

locals {
  deployed = jsondecode(var.deployed_json)
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

resource "aws_s3_bucket" "bucket" {
  bucket = "cuddly-octo-palm-tree"
  acl    = "private"
}

resource "aws_iam_instance_profile" "read-blog" {
  name = "read-blog"
  role = aws_iam_role.read-blog.name
}

resource "aws_iam_role" "read-blog" {
  name               = "read-blog"
  path               = "/"
  assume_role_policy = <<EOF
{
  "Version": "2012-10-17",
  "Statement": [
    {
      "Action": "sts:AssumeRole",
      "Principal": {
        "Service": "ec2.amazonaws.com"
      },
      "Effect": "Allow",
      "Sid": ""
    }
  ]
}
EOF
}

resource "aws_iam_policy" "read-blog" {
  name = "read-blog"

  policy = <<EOF
{
  "Version": "2012-10-17",
  "Statement": [
    {
      "Effect": "Allow",
      "Action": [
        "s3:ListBucket"
      ],
      "Resource": "${aws_s3_bucket.bucket.arn}"
    },
    {
      "Effect": "Allow",
      "Action": [
        "s3:GetObject",
        "s3:HeadObject"
      ],
      "Resource": "${aws_s3_bucket.bucket.arn}/public/*"
    },
    {
      "Effect": "Allow",
      "Action": [
        "s3:GetObject",
        "s3:HeadObject",
        "s3:PutObject"
      ],
      "Resource": "${aws_s3_bucket.bucket.arn}/cert/*"
    },
    {
      "Effect": "Allow",
      "Action": [
        "s3:PutObject"
      ],
      "Resource": "${aws_s3_bucket.bucket.arn}/logs/*"
    }
  ]
}
EOF
}

resource "aws_iam_role_policy_attachment" "read-blog" {
  role       = aws_iam_role.read-blog.name
  policy_arn = aws_iam_policy.read-blog.arn
}

resource "aws_instance" "web" {
  for_each      = { for spec in local.deployed : spec.version => spec }
  ami           = coalesce(each.value["ami"], data.aws_ami.ubuntu.id)
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

  iam_instance_profile = aws_iam_instance_profile.read-blog.name

  user_data = templatefile("init.sh", { version = each.value["version"] })

  tags = {
    Name = each.value["version"]
  }

  depends_on = [aws_internet_gateway.gw]
}

resource "local_file" "out" {
  count    = length(local.deployed) - 1
  filename = "out"
  content  = aws_instance.web[local.deployed[1]["version"]].public_ip
}

resource "local_file" "deployed" {
  filename = "deployed"
  content = jsonencode([
    for m in local.deployed :
    {
      ami     = coalesce(m.ami, aws_instance.web[m.version].ami)
      version = m.version
    }
  ])
}

resource "aws_eip" "ip" {
  instance   = aws_instance.web[local.deployed[0]["version"]].id
  depends_on = [aws_internet_gateway.gw]
}
