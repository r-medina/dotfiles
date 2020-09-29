function registry-setup() {
    registry-creds
    gicd registry
    make
    ./registry &
    gicd gh
    env GH_CLUSTER=0 ./script/server
}
