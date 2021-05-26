f = SimpleForm("esurfing-client")
f.reset = false
f.submit = false
f:append(Template("esurfing-client/log"))

return f
