whence go || exit 0

go env -w GOPRIVATE="*github.com/github/*"
go env -w GONOPROXY="*github.com/github/*"
go env -w GOPRIVATE='*github.com/github/*'
