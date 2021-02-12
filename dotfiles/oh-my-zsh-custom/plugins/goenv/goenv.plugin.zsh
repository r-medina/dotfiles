whence go > /dev/null || return

export GOPATH="/Users/ricky"

# we're only really concerned with a few go environment variables
govars=(
    GOPATH
)

for var in $govars; do
    export $var=$(go env $var)
done
