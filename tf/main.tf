terraform {
  backend "s3" {
    bucket       = "tf-state"
    key          = "blog"
    region       = "auto"
    use_lockfile = true

    # from https://developers.cloudflare.com/terraform/advanced-topics/remote-backend/
    skip_credentials_validation = true
    skip_metadata_api_check     = true
    skip_region_validation      = true
    skip_requesting_account_id  = true
    skip_s3_checksum            = true
    use_path_style              = true

    # because we _also_ use AWS as a provider, we cannot rely on the default env vars here (or there)
    access_key = var.cloudflare_s3_access_key
    secret_key = var.cloudflare_s3_secret_key
    endpoints = {
      s3 = var.cloudflare_s3_endpoint
    }
  }
  required_providers {
    cloudflare = {
      source  = "cloudflare/cloudflare"
      version = "~> 5"
    }
    aws = {
      source  = "hashicorp/aws"
      version = "~> 5.0"
    }
    dnsimple = {
      source  = "dnsimple/dnsimple"
      version = "~> 1.5"
    }
  }
}

variable "cloudflare_s3_access_key" {}
variable "cloudflare_s3_secret_key" {}
variable "cloudflare_s3_endpoint" {}
variable "cloudflare_account_id" {}

provider "aws" {
  region = "us-east-1"
}

provider "dnsimple" {
}

provider "cloudflare" {
}

locals {
  domain = "cuddly-octo-palm-tree.com"
}

resource "dnsimple_zone" "domain" {
  name   = local.domain
  active = true
}

resource "dnsimple_domain_delegation" "blog" {
  domain       = local.domain
  name_servers = ["elaine.ns.cloudflare.com", "kobe.ns.cloudflare.com"]
}

locals {
  deployed = [{"ami":"ami-0b4c74ae0c6164c48","version":"20250810-1703-924699e0"}]
}


data "aws_ami" "ubuntu" {
  most_recent = true

  filter {
    name   = "name"
    values = ["ubuntu-minimal/images/hvm-ssd-gp3/ubuntu-noble-24.04-amd64-minimal-*"]
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

resource "cloudflare_zone" "blog" {
  account = {
    id = var.cloudflare_account_id
  }
  name = "cuddly-octo-palm-tree.com"
  type = "full"
}

resource "cloudflare_zone_setting" "https" {
  zone_id    = cloudflare_zone.blog.id
  setting_id = "always_use_https"
  value      = "on"
}
