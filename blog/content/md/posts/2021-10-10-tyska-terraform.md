{:title "Tools You Should Know About: Terraform"
 :layout :post
 :tags ["tyska" "terraform"]}

## In a nutshell

From [the homepage]:

> Terraform is an open-source infrastructure as code software tool that
> provides a consistent CLI workflow to manage hundreds of cloud services.
> Terraform codifies cloud APIs into declarative configuration files.

The terms "infrastructure" and "cloud services" in that description are often
thought of meaning the obvious big ones like AWS or Microsoft Azure. Terraform
is, in fact, way broader than that.

## Why you should know about it

As a programmer in the twenty-first century, you interact with cloud services.
If you don't yet know about Terraform, you manage these services through a mix
of clicking on web UIs, `curl` commands and support tickets raised with your
infrastructure team.

There's a better way. Terraform lets you turn pretty much any web service
configuration into a text file. This is very powerful, because it means you can
manage your web services with git.

git solves a number of problems people typically have with infrastructure:
- Tracking the state of things over time.
- Keeping an audit log of who made what change.
- Building blocks for change requests through branching & merging.

## Misconceptions

### A common language

As mentioned, many people who have heard of Terraform think of it as a common
language to manage cloud resources on AWS, Azure & GCP. This is missing the
point in two ways.

First, Terraform is most definiftely not a way to paper over the differences
between different cloud providers. If you're hoping that using Terraform will
make it "easy" to switch cloud provider, you're in for a world of pain. That
way lies madness.

While Terraform does offer a single regardless of the provider, it's better to
think of it as a common syntax. Just like, say, most of these provders let you
interact with them through JSON blobs sent over HTTP.

It's nice not to have to learn a new _syntax_ whenever you have to interact
with a new provider, but you'll still have to learn all of the concepts related
to that provider, and the vast majority of Terraform code you write will still
be extremely tied into that one specific provider.

It's a common syntax, not an abstraction layer.

### Cloud infrastructure means AWS & co

While I'm sure AWS, GCP and Azure make up the majoity of Terraform usage, the
tool itself is very agnostic. Any situation where you can manage the state of
resources through an API is a good use-case for Terraform. Here are a few
examples outside the big three:

- GitHub: You can use Terraform to manage GitHub resources. That may not seem
  very useful for a single user account, but in an entreprise context, you can
  quickly build up a pretty complex setup between organizations, teams, repo
  access rights, branch protection rules, etc.
- Kubernetes: You can use Terraform to manage k8s resources. k8s comes with its
  own configuration language (YAML-based), but you'll typically need to
  provision the infrastructure _under_ your k8s cluster as well as the resources
  running _on_ it, and why should you use a different language for that?
- Artifactory: At work, we use a service called Artifactory for, as the name
  might suggest, hosting artifacts. Again, this may seem like something you can
  easily manage through a web UI at first, but from experience I can say you
  quickly end up with a few dozen reposiroties and a few dozen users and it gets
  quite hard to keep track of the web of permissions there.

There are many official and even more community-supported providers for
Terraform. If you're using any kind of cloud service (a.k.a. remote resource),
it may be worth [checking if there's a provider for it][providers].

## Feature highlights

### Text files

Terraform offers a big advantage over manual management through either API
calls or web UIs: text files enable all sorts of _process_ automation, as well
as history auditing. This is the main selling point: you can collaborate, you
can share, you can make change requests, and you can have a good view of the
current state by looking at said files.

### Common language

Some cloud providers understand the power of text files and thus provide their
own configuration language. That's great, but that means you have to learn a
new syntax for each one. Also, a lot of cloud providers do not offer anything
like this.

The language used by Terraform is by no means perfect; like any language, it
has its own set of quirks and idiosyncracies. But investing in learning it
gives you a lot of leverage given the range of services you can configure using
it.

### Drift detection / declarative approach

A perhaps more obvious way to turn a cloud configuration into a set of text
files would be to just write down a set of curl commands in a Bash script to
create the state you want to end with. Conceptually, that's a good first step
towards having some form of audit log.

However, Terraform provides you with something more, which is quite a lot of
work to build out yourself: once Terraform has created a resource, it keeps
track of it. It will detect if it changes, and offer to change it back so it
stays in think with your provided description. If you remove a resource
description from your configuration files, Terraform will also detect that and
offer to delete the corresponding resource.

### Dependency tracking

If you want to create resources that depend on each other, Terraform will
detect that and create them in the correct order (provided there is one). For
example, if you want to create a "classic" 3-tier application infrastructure,
with three separate machines (database, middleware, frontend), and your
configuration reflects the fact that the middleware needs the IP address of the
database and the frontend needs the IP address of the middleware, Terraform
will create the database first, wait until it gets an IP address assigned, then
create the middleware, and so on with the frotnend.

This type of dependency tracking works for resources across different
providers, too. If you wanted to create an AWS instance to host a website, and
then create a StatusCake configuration to monitor that machine, you could
express that in a single Terraform configuration, and Terraform would be able
to infer that the machine needs to be created first so Terraform can get its IP
and put it in the StatusCake configuration.

## Conclusion

[the homepage]: https://www.terraform.io
[providers]: https://registry.terraform.io/browse/providers
