EAPI=6


inherit  eutils gnome2-utils pax-utils unpacker xdg-utils

DESCRIPTION="${1:DESC}"
HOMEPAGE="${2:HOMEPAGE}"

SRC_URI="${3:SRC}"

SLOT="0"
KEYWORDS="amd64"
S=${WORKDIR}

DISABLE_AUTOFORMATTING="yes"

src_install() {
    dodir /
	cd "${ED}" || die
	unpacker
}

pkg_postinst() {
    gnome2_icon_cache_update
    xdg_desktop_database_update
}

pkg_postrm() {
	gnome2_icon_cache_update
	xdg_desktop_database_update
}
