# we're only really concerned with a few go environment variables
govars=(
    GOPATH
    # GOROOT
)

for var in $govars; do
    export $var=$(go env $var)
done

# GOMAXPROCS
# TODO: maybe set goproxy stuff in here?

