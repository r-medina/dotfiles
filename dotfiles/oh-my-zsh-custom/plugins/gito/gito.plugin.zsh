whence gito > /dev/null || return

gicd() {
    cd $(gito where $1)
}

gpen() {
    open $(gito url $1)
}
