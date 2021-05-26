module("luci.controller.esurfing-client", package.seeall)

function index()
	if not nixio.fs.access("/etc/config/esurfing-client") then
		return
	end

	local e = entry({"admin", "services", "esurfing-client"}, alias("admin", "services", "esurfing-client", "setting"), _("天翼校园客户端"), 50)
	e.dependent = false
	e.acl_depends={ "luci-app-esurfing-client" }

	entry({"admin", "services", "esurfing-client", "setting"}, cbi("esurfing-client/setting"), _("配置"), 1).leaf = true
	entry({"admin", "services", "esurfing-client", "log"}, form("esurfing-client/log"), _("日志"), 2).leaf = true
	entry({"admin", "services", "esurfing-client", "get_log"}, call("get_log")).leaf = true
	entry({"admin", "services", "esurfing-client", "clear_log"}, call("clear_log")).leaf = true
end

function get_log()
	luci.http.write(luci.sys.exec(
		"[ -f '/tmp/esurfing-client/esurfing-client.log' ] && cat /tmp/esurfing-client/esurfing-client.log"))
end

function clear_log()
	luci.sys.call("echo '' > /tmp/esurfing-client/esurfing-client.log")
end