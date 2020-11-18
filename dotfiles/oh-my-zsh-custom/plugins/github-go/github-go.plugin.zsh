whence go > /dev/null || return

go env -w GOPRIVATE="*github.com/github/*"
go env -w GONOPROXY="*github.com/github/*"
