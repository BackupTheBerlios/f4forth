#!/bin/sh
# F4,F4A Copyright (C) 2003 by h-peter recktenwald <f4@lxhp.in-berlin.de>

unset d f g
#pwd>/dev/tty
d=$(dirname ${0})
d=$(dirname ${d})/i/version.inc

f () { [ "$((${1}))" -lt "$((10))" ]&&echo -n 0$((${1}))||echo -n ${1}; }

# NASM char praefix for "define"
ss='%'
# AS char praefix for "define"
ss='#'

# -------------------------------------------------------------------

[ -z "${2}" ]&&s=${d}||s=${2}

REL="$((10#$(grep "${ss}define\ REL\>" ${s}|cut -d\  -f3||echo 0)))"
VER="$((10#$(grep "${ss}define\ VER\>" ${s}|cut -d\  -f3||echo 0)))"
EXT="$((10#$(grep "${ss}define\ EXT\>" ${s}|cut -d\  -f3||echo 0)))"
TAG="$((10#$(grep "${ss}define\ TAG\>" ${s}|cut -d\  -f3||echo 0)))"

#echo R:${REL} V:${VER} E:${EXT} T:${TAG}

case ${1} in
    r)		# r
    echo ${REL};;
    v)		# v
    echo ${VER};;
    x)		# x
    echo ${EXT};;
    t)		# t
    echo ${TAG};;
    r.v)	# r.v
    echo ${REL}.${VER};;
    r.v.x.t)	# rvx
    echo ${REL}-${VER}-${EXT}+${TAG};;
    rvx)	# rvx
    echo ${REL}${VER}${EXT};;
    rvxt)	# rvx
    f ${REL};f ${VER};f ${EXT};f ${TAG};echo;;
    v+|x+|r+|t+|tt|vv|xx|rr)	# next REL/VER/EXT number
        case ${1} in
	    r+|v+|x+)	( make uninstall >/dev/null 2>&1||true );;
	esac
	case ${1} in
	    r+|rr|v+|vv|x+|xx)
	    TAG=0
	    cp ${s} ${s}.old;;
	esac
        case ${1} in
	    r+|rr)	g=REL;EXT=00;VER=00;;	# r+
	    v+|vv)	g=VER;EXT=00;;		# v+
	    x+|xx)	g=EXT;;			# x+
	    t+|tt)	g=TAG;;			# t+
	esac
	export ${g}="$((${!g}+1))"
	eval [ "$((${EXT}))" -lt "$((10))" ]&&EXT=0$((${EXT}))
	eval [ "$((${TAG}))" -lt "$((10))" ]&&TAG=0$((${TAG}))
	echo "${ss}define REL ${REL}"  > ${s}
	echo "${ss}define VER ${VER}" >> ${s}
	echo "${ss}define EXT ${EXT}" >> ${s}
	echo "${ss}define TAG ${TAG}" >> ${s}
# cpp numeric stgs:
	echo "${ss}define VERNUM 0x${REL}${VER}${EXT}${TAG}" >> ${s}
# AS .version:
	[ "${ss}" = "%" ]||echo ".version \"${REL}.${VER}.${EXT}.${TAG}\"" >> ${s}
# AS numeric cons:
	[ "${ss}" = "%" ]||echo ".equ VERNUM,0x${REL}${VER}${EXT}${TAG}" >> ${s}
	;;
    -)
    echo ${REL}.${VER}.${EXT};;
    *-*)
	echo "
increment lib4th version number by one:
	rr:	REL
	vv:	VER
	xx:	EXT
uninstall current lib4th version and increment version number:
	r+:	REL \\
	v+:	VER  )TAG := 0
	x+:	EXT /
	t+:	TAG
echo lib4th version number to stdout:
	r: echo	REL
	v:	VER
	x:	EXT
	r.v:	REL.VER
	r.v.x:	REL.VER.EXT
	rvx	RELVEREXT
	rvxt	REL-VER-EXT+TAG
	-	REL.VER.EXT
	-none-	REL.VER.EXT.TAG
optional 2nd arg
	filename where to read the version data from,
	default is \"./i/version.inc\"."
	;;
    *)
    echo ${REL}.${VER}.${EXT}.${TAG};;
esac
