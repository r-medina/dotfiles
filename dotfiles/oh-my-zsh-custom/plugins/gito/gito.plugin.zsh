whence gito > /dev/null || return

gicd() {
    cd $(gito where $1)
}

gpen() {
    open $(gito url $1)
}

gmk() {
    local dirName=$1
    local self=$(gito self)

    cd $self

    mkdir $dirName
    cd $dirName
    git init
}
