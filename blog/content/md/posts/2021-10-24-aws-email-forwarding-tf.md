{:title "Forwarding AWS SES emails with Terraform"
 :layout :post
 :tags ["terraform" "aws"]}

A few weeks ago, I had a bit of a scramble when I realized, on Saturday, that
the corporate emails I'm responsible for would suddenly stop working on the
next Monday. I had two days to figure out a solution.

A few weeks prior, I had decided to switch DNS provider. I had been unhappy
with the service I was getting for about a year, but DNS is a bit of a
dangerous thing to tinker with, so I'd put it off. It also has a tendency to
fade into the background, as (at least at the level of complexity I'm at) it's
very slow-changing.

Changing the DNS setup itself was not too difficult after a bit of research. I
set up a new DNS zone on Route53, copying most fo the existing entries, and
started the transfer from my old registrar to AWS.

The transfer takes about ten days for various reasons. On Friday, I received
confirmation that the switch would be executed on Monday. On Saturday, I
suddenly realized that my old registrar was providing me with a bit more than
DNS services: it was also forwarding emails. It was too late to cancel the
transfer.

Email forwarding is a service a lot of DNS providers offer, but Route53 does
not. So I started frantically researching how to actually do that with AWS, and
since I had a bit of trouble putting all the pieces together, I decided to
record the results of that research here.

## DNS overview

This article is not about DNS per se, but DNS plays a role, so in case any
reader is completely unfamiliar with how DNS works (and for my future self,
given how rarely I need to interact with it), here is a very quick overview.

A DNS "zone" is a set of records. There are various types of records; in
general, a record is essentially a line of text of the form:

```
domain ttl class type data
```

For the purposes of this blog post, the class is always IN (standing for
"internet"), and the type is one of `A`, `MX`, `NS`, or `TXT`. There is only
one record of a given type for a given domain. `A` records indicate the IP
address of the server corresponding to the listed domain; `MX` records indicate
the address (usually as a domain name) of the mail server to send emails @ that
domain to; `NS` records indicate the servers for the listed domain; and `TXT`
records are free-form text. The TTL (time to live) is a number of seconds
during which the entry can be cached.

The DNS zone for a given domain can include entries for any subdomain, which is
why the domain is part of the record. For example, these two records are part
of the one zone for `cuddly-octo-palm-tree.com`:

```
cuddly-octo-palm-tree.com.        30    IN    A    52.204.159.248
www.cuddly-octo-palm-tree.com.    30    IN    A    52.204.159.248
```

A DNS server is a system that serves DNS records; the query comes in the form
of domain + type, e.g. "please give me the A record for example.com". If the
server has that in its own cache, it checks when it got it and whether that's
more seconds ago than the TTL. If it is not, it means the entry is still valid,
so it serves it.

If the server does not have a fresh-enough response, it needs to fetch it.
There may be multiple levels of caching here, but ultimately we'll reach a
point where we need to query the authority on that domain. This is where the
hierarchical nature of DNS appears: `example.com` is a subdomain of `com`.

In practice, what that means is that if you need an IP address for
`images.example.com`, and you're a DNS that doesn't have another DNS server
configured as your cache, you'll need to reach out to the authoritative server
for `com`. Fortunately, that one is well-known and probably hard-coded, so you
know how to reach it. You'll probably start by asking it for the record you
actually want, that is the `A` record for `images.example.com`, and it will
tell you it doesn't have it. So you'll ask for the `NS` record of
`images.example.com`, and it still won't have it. Eventually, it will give you
the `NS` record for `example.com`, which will tell you what _other_ DNS server
you need to contact for that. That server may in turn have an `À` entry for
`images.example.com` directly, or further have an `NS` record sending you to
yet another server. That third one, though, will be the end of the road: either
it has an `A` record, or there is no `A` record for that domain.

Top-level domains (TLDs) are too high on the hierarchy to deal with individuals
directly. Therefore, though at a technical level there isn't much of a
difference between the redirection from the `com` server to the `example.com`
server and the one from `example.com` to `images.example.com`, there's a big
difference in process.

In order to register a domain name, you need to go through a _registrar_. From
a technical perspective, all a registrar does is ensure that the corresponding
TLD server has an entry for your domain that redirects to a server _you give
them_[^registrar]. They're an intermediaray between you and whoever actually
owns the TLD.

[^registrar]: On an administrative/legal level they do a lot more.

Registering a domain name is thus not very useful unless you also have a server
hosting your zone. Most DNS providers will serve both roles and do the "tell
your registrar about your zone" step for you, which may make it a bit hard to
distinguish those two roles, but AWS keeps them well separated. In particular,
there does not seem to be an API for the registrar part of Route53.
