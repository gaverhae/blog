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

resource "aws_s3_bucket" "bucket" {
  bucket = "cuddly-octo-palm-tree"
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
