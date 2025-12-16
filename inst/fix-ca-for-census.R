## A (hopefully temporary) workaround for an SSL certificate issue until
## Cloudflare changes he certifcate chain it uses, or census changes something.
## Note: Try to run without it to see if this temporary workaround is still needed.
## source: https://community.cloudflare.com/t/cloudflares-chain-root-ca-removed-from-todays-certifi-update/792978
# one‑off: fetch latest Mozilla CA bundle (~200 KB)
download.file(
  "https://curl.se/ca/cacert.pem",      # verified source
  destfile = "~/.certs/cacert.pem",
  mode = "wb"  # binary
)
download.file(
  "https://www.tbs-x509.com/Comodo_AAA_Certificate_Services.crt",
  destfile = "~/.certs/aaa_root.pem",
  mode = "wb"  # binary
)
download.file(
  "https://ssl-tools.net/certificates/5cd5961e1efd90ec72bbccc99a0f66ab1f61d464.pem" ,
  destfile = "~/.certs/sslcom_ecc_r2.pem",
  mode = "wb"  # binary
)

# Append the pem files to the system's CA bundle
file.append(
  path.expand("~/.certs/cacert.pem"),
  path.expand("~/.certs/aaa_root.pem")
)

# Add new line to the end of the CA bundle
cat("\n", file = "A")
file.append(
  path.expand("~/.certs/cacert.pem"),
  "A"
)

# Append the second pem file to the system's CA bundle
file.append(
  path.expand("~/.certs/cacert.pem"),
  path.expand("~/.certs/sslcom_ecc_r2.pem")
)

# point libcurl at it for the rest of the session
Sys.setenv(CURL_CA_BUNDLE = path.expand("~/.certs/cacert.pem"))

