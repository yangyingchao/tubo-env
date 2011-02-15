# Copyright 1999-2010 Gentoo Foundation
# Distributed under the terms of the GNU General Public License v2
# $Header: ^&^ $

DESCRIPTION="(>>FILE<<)"
DOCS="AUTHORS ChangeLog NEWS README TODO"
EAPI="2"
HOMEPAGE="http://www.bukengnikengshui.com/(joke)"
IUSE="doc"
KEYWORDS="amd64 x86"
LICENSE="LGPL-2"
SLOT="0"
SRC_URI="http://abc.org/${P}.tar.bz2"

src_configure(){

    local myconf

    myconf="${myconf} --prefix=/usr --sysconfdir=/etc"

    econf ${myconf}

}

src_compile(){
    emake -j3 || die "Make failed."
}

src_install() {
    DESTDIR={D} einstall || die "Failed to install"
}
