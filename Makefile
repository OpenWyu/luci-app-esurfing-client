#
# Copyright (C) 2020 LengSword <ylengsword@gmail.com>
#
# This is free software, licensed under the MIT License.

include $(TOPDIR)/rules.mk

PKG_NAME:=luci-app-esurfing-client
PKG_VERSION:=1.0
PKG_RELEASE:=1

PKG_BUILD_DIR:=$(BUILD_DIR)/$(PKG_NAME)

include $(INCLUDE_DIR)/package.mk

define Package/luci-app-esurfing-client
	SECTION:=luci
	CATEGORY:=LuCI
	SUBMENU:=3. Applications
	TITLE:=LuCI app for China Telecom's esurfing school client
	PKG_MAINTAINER:=LengSword <ylengsword@gmail.com>
	PKGARCH:=all
	DEPENDS:=+luci-base +luarocks
endef

define Package/luci-app-esurfing-client/description
	LuCI Interface for China Telecom's esurfing school client
endef

define Build/Prepare
	po2lmo ./po/zh-cn/esurfing-client.po $(1)/usr/lib/lua/luci/i18n/esurfing-client.zh-cn.lmo
endef

define Build/Compile
endef

define Package/luci-app-esurfing-client/conffiles
/etc/config/esurfing-client
endef

define Package/luci-app-esurfing-client/prerm
#!/bin/sh
if [ -z "$${IPKG_INSTROOT}" ]; then
     /etc/init.d/esurfing-client disable
     /etc/init.d/esurfing-client stop
uci -q batch <<-EOF >/dev/null 2>&1
	delete ucitrack.@esurfing-client[-1]
	commit ucitrack
EOF
fi
exit 0
endef

define Package/luci-app-esurfing-client/postinst
#!/bin/sh
	/etc/init.d/esurfing-client enable >/dev/null 2>&1
	enable=$(uci get esurfing-client.esurfing-client.enabled 2>/dev/null)
	if [ "$enable" == "1" ]; then
		/etc/init.d/esurfing-client enable >/dev/null 2>&1
	fi
	rm -f /tmp/luci-indexcache
	rm -rf /tmp/luci-modulecache/
	exit 0
endef

define Package/luci-app-esurfing-client/install
    $(INSTALL_DIR) $(1)/usr/lib/lua/luci
	cp -pR ./luasrc/* $(1)/usr/lib/lua/luci
	$(INSTALL_DIR) $(1)/
	cp -pR ./root/* $(1)/
	$(INSTALL_DIR) $(1)/usr/lib/lua/luci/i18n
	$(INSTALL_DATA) ./po/zh-cn/esurfing-client.zh-cn.lmo $(1)/usr/lib/lua/luci/i18n
endef

$(eval $(call BuildPackage,luci-app-esurfing-client))
