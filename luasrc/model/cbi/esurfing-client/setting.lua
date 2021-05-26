local nwm = require "luci.model.network".init()
local networks = nwm:get_wan_networks()


m = Map("esurfing-client")
m.title = translate("天翼校园客户端")
m.description = translate("电信天翼校园客户端(五邑大学版)")


s = m:section(TypedSection, "esurfing-client")
s.addremove = false
s.anonymous = true

enable = s:option(Flag, "enabled", translate("启用"))
enable.rmempty = false
enable.default = 0

name = s:option(Value, "username", translate("用户名"))
name.rmempty = false

pass = s:option(Value, "password", translate("密码"))
pass.rmempty = false
pass.password = true

school = s:option(ListValue, "school", translate("学校类型"))
school.rmempty = true
school.description = translate("对于邑大校园网，客户端所需参数会自动获取<br/>其他学校的校园网则需要获取对应的参数填入以下设置")
school:value("wyu", translate("五邑大学"))
school:value("others", translate("其他学校"))
school.default = "wyu"

macaddr = s:option(Value, "macaddr", translate("设备MAC地址"))
macaddr.rmempty = true
macaddr.datatype = "macaddr"
macaddr.description = translate("系统会自动获取路由器设备WAN口的MAC地址作为该值(格式: XX:XX:XX:XX:XX:XX)")
macaddr:depends("school", "others")

clientip = s:option(Value, "clientip", translate("客户端IP地址"))
clientip.rmempty = true
clientip.datatype = "ip4addr"
clientip.description = translate("系统会自动获取路由器设备WAN口的IP地址作为该值<br/>如果是邑大校园网, 则会自动从认证网站获取clientip(故一般仅用于下线客户端)")
clientip:depends("school", "others")

for _, net in ipairs(networks) do
    local mac = net:get_interface():mac()
    macaddr:value(mac, "%s (%s)" %{ mac, net:name()})
    macaddr.default = mac

    clientip:value(net:ipaddr(), "%s (%s)" %{ net:ipaddr(), net:name()})
    clientip.default = net:ipaddr()
end

nasip = s:option(Value, "nasip", translate("NASIP地址"))
nasip.rmempty = true
nasip.datatype = "ip4addr"
nasip.description = translate("如果是邑大校园网, 连上校园网后会自动从当前校园网环境获取nasip, 否则使用该默认值<br/>其他学校的校园网则需要获取其nasip地址填入这里, 一般是固定值")
nasip.default = "119.146.175.80"
nasip:depends("school", "others")


schoolid = s:option(Value, "schoolid", translate("学校ID"))
schoolid.rmempty = true
schoolid.description = translate("默认值是邑大校园网的schoolid<br/>其他学校的校园网则需要获取其schoolid填入这里, 一般是固定值")
schoolid.default = "1414"
schoolid:depends("school", "others")

secretkey = s:option(Value, "secretkey", translate("密钥"))
secretkey.rmempty = true
secretkey.description = translate("固定密钥值, 用于获取验证码和登录的校验")
secretkey.default = "Eshore!@#"
secretkey:depends("school", "others")

return m
