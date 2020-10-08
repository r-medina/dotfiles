function iterlines() {
    (( $# < 2 )) && { echo "Usage: iterlines '<text>' <callback>"; return; }
    local text=$1
    local func=$2

    setopt LOCAL_OPTIONS SH_WORD_SPLIT

    local IFS=$'\n'
    for line in $text; do
        $func $line
    done
}

# pswd puts the password of the named service into the clipboard.
# If there's a one time password, it will be copied into the
# clipboard after 5 seconds
function pswd() {
    (( $# < 1 )) && { echo "Usage: pswd <service>"; return }
    local service=$1

    (( ! oploggedin )) && opsignin
    
    op get item $service | jq -r '.details.fields[] | select(.designation=="password").value' | pbcopy

    # TODO: exit if no password
    
    ( sleep 5 && op get totp $service | pbcopy
      sleep 10 && pbcopy < /dev/null 2>&1 & ) &!

}

function oploggedin() {
    op list users &> /dev/null
}

# connects to Viscosity VPN
function vc() {
    (( $# < 1 ))  && { echo "Usage: vc <vpn>"; return; }
    local vpn=$1

    osascript -e "tell application \"Viscosity\" to connect \"$vpn\""
}
