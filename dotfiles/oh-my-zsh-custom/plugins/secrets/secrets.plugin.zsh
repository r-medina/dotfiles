alias os='! oploggedin && opsignin'

function get-secret() {
    (( $# < 2 )) && { echo "Usage: get-secret <namespace> <which_secret>"; return }
    local namespace=$1
    local which_secret=$2

    op get item $namespace |
	jq -r ".details.sections[]?.fields[]? | select(.t==\"$which_secret\").v"
}

function azure-creds() {
    os
    export AZURE_STORAGE_ACCOUNT=$(get-secret gh AZURE_STORAGE_ACCOUNT)
    export AZURE_STORAGE_KEY=$(get-secret gh AZURE_STORAGE_KEY)
}

function registry-creds() {
    os
    export PKGS_TOKEN=$(get-secret gh PKGS_TOKEN)
    export AWS_ACCESS_KEY=$(get-secret gh AWS_ACCESS_KEY)
    export AWS_SECRET_KEY=$(get-secret gh AWS_SECRET_KEY)
    export GH_API_TOKEN=$(get-secret gh GH_API_TOKEN)
}

function spotify-app-creds() {
    os
    export SPOTIFY_ID=$(get-secret spotify id)
    export SPOTIFY_SECRET=$(get-secret spotify secret)
}
