function azure-creds() {
    export AZURE_STORAGE_ACCOUNT=$(get-gh-secret AZURE_STORAGE_ACCOUNT)
    export AZURE_STORAGE_KEY=$(get-gh-secret AZURE_STORAGE_KEY)
}

function registry-creds() {
    export PKGS_TOKEN=$(get-gh-secret PKGS_TOKEN)
    export AWS_ACCESS_KEY=$(get-gh-secret AWS_ACCESS_KEY)
    export AWS_SECRET_KEY=$(get-gh-secret AWS_SECRET_KEY)
    export GH_API_TOKEN=$(get-gh-secret GH_API_TOKEN)
}

# getting this to work requires having an item in your 1password with the name "gh"
function get-gh-secret() {
    (( $# < 1 )) && { echo "Usage: get-gh-secret <which_secret>"; return }
    local which_secret=$1

    (( ! oploggedin )) && opsignin

    op get item gh | jq -r ".details.sections[]?.fields[]? | select(.t==\"$which_secret\").v"
}

function spotify-app-creds() {
    (( ! oploggedin )) && opsignin

    export SPOTIFY_ID=$(op get item spotify | jq -r ".details.sections[]?.fields[]? | select(.t==\"id\").v")
    export SPOTIFY_SECRET=$(op get item spotify | jq -r ".details.sections[]?.fields[]? | select(.t==\"secret\").v")
}
