//; =============================================================================
//; F4,F4A Copyright (C) 2003 by h-peter recktenwald <f4@lxhp.in-berlin.de>
//; =============================================================================
//; f4-asm-cfg.h - configureable asm vari
//; =============================================================================
//;
#ifndef F4ASMCFG_H
#define F4ASMCFG_H
//;
//; =============================================================================
//;
//; -- f4 asm source debugging --------------------------------------------------
//;
#ifndef _dh_
#define _dh_     1
// vari via make-fs
#else
#undef  _dh_
#define _dh_     0
#endif
//;#define _dh_     0	   	cpl dummy headers for header-less words
#ifndef _oh_
#define _oh_     1	// vari via make-fs
#endif
//;#define _oh_     1	   	cpl headers for optimizing {literal} oprs
#define _tf_    1
//;#define _tf_    1	   	'tdb' mode w. eflags display
#define _i3_    0
//;#define _i3_    0		'int3'  debugger breakpt 4th word
#define _cy_	  0
//;#define _cy_	  0		debug: include int $3 instr-s and intial waiting loop
#define _tm_     0
//;#define _tm_     0		tib words debugging, 't'
#define _tq_     1
//;#define _tq_     1		w. 'tdb?'

// ==============================================================================

#ifndef _f4min_ // == fully featured set-up ======================================

//; -- cfg conditional kernel assemby --
//; -- kernel -------------------------------------------------------------------
#define _c4_     0
//;#define _c4_     0		subroutine calls to hi-level 4th words
#define _nv_	  1
//;#define _nv_	  1		vectored '[number]'
//; -- f4 additional words ------------------------------------------------------
#define _pa_	  1
//;#define _pa_	  1		cpu i/o port access
#define _ck_      1
//;#define _ck_      1		address of channels kbd char buffer
#define _to_      1
//;#define _to_      1		'=:' store to/define a constant
#define _0a_      1
//;#define _0a_      1		'0and', '1or', '0max' word headers
#define _sb_      1
//;#define _sb_      1		dslb, 2<<, 2>>, qslb, 4<<, 4>>, q2/, uq2/, q2*
#define _fc_     1
//;#define _fc_     1		factorial
#define _sq_      1
//;#define _sq_      1		square root, gcd, u^
#define _sh_	  1
//;#define _sh_	  1		xec shell command
#define _k2_	  1
//;#define _k2_	  1		key>alpha { ddup uc-lc =/= }
#define _al_    1
//;#define _al_    1		align, 2^, pow2, 1abs, u+, -0
#define _hx_	10
//;#define _hx_	10	   	4th headers for internal primitives
#define _dx_	1
//;#define _dx_	1	   	doubles extn: 3drop, 3dup, 4drop, 4dup, 
//;			   	d<, du<, d=, df/mod, qm/mod, qfm/mod, uqd/mod
#define _ld_	1
//;#define _ld_	1	        double locals: l2@, l2!, l2!0, l2@0
#define _lx_	1
//;#define _lx_	1	   	locals extn: l@, l!, l@0, l!0
#define _mx_	1
//;#define _mx_	1	   	multiple (counted) cells: n@, n!
#define _bn_	1
//;#define _bn_	1		counted integer logic & arithm opr; implies _mx_
#define _rx_	1
//;#define _rx_	1	   	for rational numbers extn: 8*, 8/, 16*, 16/
#define _rv_	 00
//;#define _rv_	 00	   	ranum voc testing - cannot use more vocs in kernel, why?
//; -- more words ---------------------------------------------------------------
#define _gs_	0
//;#define _gs_	0   	odds&ends,[=synonymous words], implies _dx_
#define _af_   1
//;#define _af_   1		'-find' in all vocs, following 'voc-link'
#define _ie_  00
//;#define _ie_  00		'index', for screen-files
#define _ti_   0
//;#define _ti_   0		'triad', for screen-files
#define _tu_    0
//;#define _tu_   0		'thru', for screen-files

#else  // == _f4min_ ============================================================

// -- minimal set-up w. _f4min_ passed from <make-f4a> --------------------------
#define _c4_      0
//;#define _c4_      0		subroutine calls to hi-level 4th words
#define _nv_	  1
//;#define _nv_	  1		vectored '[number]'
//; -- f4 additional words ------------------------------------------------------
#define _pa_	  00
//;#define _pa_	  00		cpu i/o port access
#define _ck_      00
//;#define _ck_      00		address of channels' kbd char buffer
#define _to_      00
//;#define _to_      00		'=:' store to/define a constant
#define _0a_     1
//;#define _0a_     1		'0and', '1or', '0max' word headers, '-!'
#define _sb_      00
//;#define _sb_      00		dslb, 2<<, 2>>, qslb, 4<<, 4>>, q2/, uq2/, q2*
#define _fc_      00
//;#define _fc_      00		factorial
#define _fx_	 1
//;#define _fx_	 1	    	F4 rational numbers <-> ieee-754 floats conversion
#define _sq_      00
//;#define _sq_   		00square root, gcd, u^
#define _sh_	 1
//;#define _sh_	 1		xec shell command
#define _k2_	  00
//;#define _k2_	  00		key>alpha { ddup uc-lc =/= }
#define _al_    0
//;#define _al_    0		align, 2^, pow2, 1abs, u+, -0
#define _hx_	0
//;#define _hx_	0	   	4th headers for internal primitives
#define _dx_	0
//;#define _dx_	0	   	doubles extn: 3drop, 3dup, 4drop, 4dup, cswap
//;			   	d<, du<, d=, df/mod, qm/mod, qfm/mod, uqd/mod
#define _ld_	0
//;#define _ld_	0	        double locals: l2@, l2!, l2!0, l2@0
#define _lx_	0
//;#define _lx_	0	   	locals extn: l@, l!, l@0, l!0
#define _mx_	0
//;#define _mx_	0	   	multiple (counted) cells: n@, n!
#define _bn_	0
//;#define _bn_	0		counted integer logic & arithm opr; implies _mx_
#define _rx_	0
//;#define _rx_	0	   	for rational numbers extn: 8*, 8/, 16*, 16/
#define _rv_	 00
//;#define _rv_	 00	   	ranum voc testing - cannot use more vocs in kernel - why?
//; -- more words ---------------------------------------------------------------
#define _gs_	0
//;#define _gs_	0	   	odds&ends,[=synonymous words]; zeroed if _dx_ = 0
//;				'>swap<' [=cswap]
#define _af_    1
//;#define _af_    1		'-find' in all lists, following 'voc-link'
#define _ie_   0
//;#define _ie_   0		'index', for screen-files
#define _ti_   0
//;#define _ti_   0		'triad', for screen-files
#define _tu_    0
//;#define _tu_    0		'thru', for screen-files
//; =============================================================================
#endif	/* not _f4min_ */
//; -----------------------------------------------------------------------------
// cons accordingly modified w. conditional out-commenting defn's, at eof:
//;	 _bn_ implies _mx_
//;	 _mx_ implies _dx_
//;	 _ld_ implies _lx_
//;	 _rx_ implies _fx_ _dx_, _ld_, _lx_, _mx_, _sq_, _sb_
//;	 _gs_ requires _dx_
//; =============================================================================
//;
#endif /* F4ASMCFG_H */
// =========================================================================== //
// f4-asm-cfg.h <eof>
