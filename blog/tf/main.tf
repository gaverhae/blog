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

  user_data = file("init.sh")

  depends_on = [aws_internet_gateway.gw]

}

resource "aws_eip" "ip" {
  instance   = aws_instance.web.id
  depends_on = [aws_internet_gateway.gw]
}
