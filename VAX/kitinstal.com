$ ON CONTROL_Y THEN vmi$callback control_y
$ ON WARNING THEN GOTO err_exit
$!
$! KITINSTAL.COM for DECWRL MODULA-2
$  vsn = "V1.1"				! version of this kit
$!
$! Greg Keller		4/22/87		Initial Version
$! Chuck Doucette	9/17/87		Final Version
$! Joel McCormack			(Actual "maintainer")
$!
$ maint = "aitg::doucette,decwrl::joel"	! maintainers address
$ node = F$GETSYI("NODENAME")		! who is installing this
$! This assumes DECNET is active (usually a safe assumption)
$ IF node .EQS. "" THEN -
  node = F$TRNLNM("SYS$NODE") - "::"
$!
$ IF p1 .EQS. "VMI$_INSTALL" THEN GOTO install
$    EXIT vmi$_unsupported
$!
$install:
$!
$! You must have some version of SYS$LIBRARY:VAXCRTL.OLB before you can
$! install this kit.
$!
$ IF F$SEARCH("SYS$LIBRARY:VAXCRTL.OLB") .NES. "" THEN GOTO check_space
$ vmi$callback message e novaxcrtl -
"SYS$LIBRARY:VAXCRTL.OLB must exist before you can install DECWRL Modula-2."
$ EXIT vmi$_failure
$!
$check_space:
$!
$! This kit needs 3400 free blocks on system device
$!
$ vmi$callback check_net_utilization dmod_size 3400
$ IF .NOT. dmod_size THEN vmi$callback message e nospace -
     "System disk does not contain enough free blocks to install DECWRL MODULA-2"
$ IF .NOT. dmod_size THEN EXIT vmi$_failure
$ vmi$callback set purge ask
$ TYPE SYS$INPUT

******************************************************************************
                This kit installs the DECWRL MODULA-2 compiler
******************************************************************************

	The next prompt will ask for the disk and directory where 
	you want to install the files.  If DECWRL MODULA-2 has never
	been installed before, you can hit <CR> and the default will 
	be SYS$COMMON:[DMOD]

	If you have installed DECWRL MODULA-2 in the past and the 
	logical name DMOD$LIB is assigned, you can hit <CR> and the 
	location you selected in the original installation will be
	where the new compiler files will be placed.

	If you do not use the defaults and answer the prompt with a 
	logical, you must put a ":" immediately after the logical,
	or the logical will not be translated correctly. 

$ default_change == "true"
$ dmod_loc = F$TRNLNM("DMOD$LIB","LNM$SYSTEM")
$ IF dmod_loc .EQS. "" THEN dmod_loc = "SYS$COMMON:[DMOD]"
$!
$ask_loc:
$ TYPE SYS$INPUT
	Please enter the device and directory where you want the 
	DECWRL MODULA-2 compiler files to be installed. Do NOT 
	answer this prompt with DMOD$LIB or unpredictable results
$ 	vmi$callback ask new_loc "	will occur. " 'dmod_loc'
$ WRITE SYS$OUTPUT ""
$!
$! Create the library directory if it doesn't exist
$!
$ dmod_loc = F$PARSE(new_loc) 
$ IF dmod_loc .NES. "" THEN GOTO lib_exists
$    vmi$callback message e nodir "''new_loc' does not exist"
$    vmi$callback ask yesno "Do you wish to create directory ''new_loc'" "YES" B
$    IF .NOT. yesno THEN GOTO ask_loc 
$!
$ vmi$callback create_directory user 'new_loc' /VERSION_LIMIT=3/OWNER=SYSTEM/protection=w:re
$ dmod_loc = "''F$PARSE(new_loc,,,"DEVICE")'''F$PARSE(new_loc,,,"DIRECTORY")'"
$ GOTO build_startup
$!
$lib_exists:
$ dmod_loc = "''F$PARSE(dmod_loc,,,"DEVICE")'''F$PARSE(dmod_loc,,,"DIRECTORY")'"
$ WRITE SYS$OUTPUT "	The compiler will be installed at ''dmod_loc'"
$ vmi$callback ask yesno "	is this correct" "YES" B 
$ IF .NOT. yesno THEN GOTO ask_loc
$!
$! Build, Provide and Execute startup file
$!
$build_startup:
$ OPEN/WRITE strt vmi$kwd:DMOD_STARTUP.COM
$ WRITE strt "$!"
$ WRITE strt "$! Startup file for DECWRL MODULA-2 compiler
$ WRITE strt "$!"
$ WRITE strt "$ ASSIGN/SYSTEM/NOLOG 	''dmod_loc' 	DMOD$LIB"
$ CLOSE strt
$!
$ SET FILE/PROTECTION=(S:RWE,O:RWED,G:RE,W:RE) vmi$kwd:*.*;*
$!
$ vmi$callback provide_file dmstart DMOD_STARTUP.COM vmi$root:[SYSMGR] K
$ @dmstart
$!
$ TYPE SYS$INPUT

  System Manager:

        The startup file DMOD_STARTUP.COM has been placed in 
	SYS$MANAGER:  Be sure to edit the system startup file to 
	include the line:

	$ @SYS$MANAGER:DMOD_STARTUP

	If this is a cluster installation, this startup file must be run
	on the other cluster nodes before the files will be available.

	The startup file contains the following four lines:

$ WRITE SYS$OUTPUT "$!"
$ WRITE SYS$OUTPUT "$! Startup file for DECWRL MODULA-2 compiler"
$ WRITE SYS$OUTPUT "$!"
$ WRITE SYS$OUTPUT "$ ASSIGN/SYSTEM/NOLOG 	''dmod_loc' 	DMOD$LIB"
$ TYPE SYS$INPUT

	
	After successful installation, a DCL foreign command symbol 
	will need to be defined to invoke the compiler images.  
	
	$ MOD :== $DMOD$LIB:MOD.EXE

$!
$! Move the files to the target directory
$!
$! Standard Library Definition Modules
$ vmi$callback provide_file dm STORAGE.DEF 'dmod_loc'
$ vmi$callback provide_file dm BITOPERATIONS.DEF 'dmod_loc'
$ vmi$callback provide_file dm ERRNO.DEF 'dmod_loc'
$ vmi$callback provide_file dm IO.DEF 'dmod_loc'
$ vmi$callback provide_file dm LONGMATH.DEF 'dmod_loc'
$ vmi$callback provide_file dm MATH.DEF 'dmod_loc'
$ vmi$callback provide_file dm MEMORY.DEF 'dmod_loc'
$ vmi$callback provide_file dm PARAMETERS.DEF 'dmod_loc'
$ vmi$callback provide_file dm STRINGS.DEF 'dmod_loc'
$ vmi$callback provide_file dm SYSTEM.DEF 'dmod_loc'
$ vmi$callback provide_file dm UNIX.DEF 'dmod_loc'
$! Compiler Executables
$ vmi$callback provide_image dm MOD.EXE 'dmod_loc'
$ vmi$callback provide_image dm MOD2.EXE 'dmod_loc'
$ vmi$callback provide_image dm XLATE.EXE 'dmod_loc'
$! Preprocessor Utility (for stand-alone usage)
$ vmi$callback provide_image dm PREP.EXE 'dmod_loc'
$! Documentation
$ vmi$callback provide_file dm OVERVIEW.TXT 'dmod_loc'
$ vmi$callback provide_file dm VMS_MOD.TXT 'dmod_loc'
$ vmi$callback provide_file dm PREPROCESS.TXT 'dmod_loc'
$ vmi$callback provide_file dm REFMAN.PSF 'dmod_loc'
$!
$!	The following lines attempt to find out if target machine has
$!	VAX C RTL v2.3 or later which has a system() call
$!	or VAX C RTL v2.2 or earlier which doesn't have a system() call.
$!
$!	This will decide which version of the Modula-2 library to install.
$!	It is only complicated because it (VAX C RTL on the target machine)
$!	may not be the same version as is running on the source machine;
$!	so, both (Modula-2 run-time) libraries are available and we provide
$!	one of them (perhaps with renaming).
$!
$!	Some lines below are commented out because they depend on which
$!	version of VAX C RTL we have installed on the source machine.
$!
$ LIBRARY/LIST=vmi$kwd:VAXCRTL.LIS/NAMES SYS$LIBRARY:VAXCRTL.OLB
$ DEFINE/USER SYS$OUTPUT NL:
$ DEFINE/USER SYS$ERROR NL:
$ SEARCH/EXACT vmi$kwd:VAXCRTL.LIS "SYSTEM"
$ ! A normal successful completion is 1 which means that "SYSTEM" was found.
$ IF $STATUS .EQ. 1 THEN goto new
$!	This means that the target machine has v2.2 (or earlier) of VAX C RTL
$ OLD:
$!	Use this line if the source machine has v2.3 of VAX C RTL
$!	This will rename the old version of the modula-2 run-time library (.OLD)
$!	as the current version of the library (.OLB).
$ rename vmi$kwd:MODLIB.OLD vmi$kwd:MODLIB.OLB
$ TYPE SYS$INPUT

		Based on an analysis of a library listing of
	SYS$LIBRARY:VAXCRTL.OLB, this COM file has come to the conclusion
	that you have version 2.2 or earlier of VAX C RTL. This version
	of the VAX C RTL has no system() call. Therefore, a version of the
	Modula-2 run-time library has been provided which comes with a
	system() call. When you install version 2.3 of VAX C RTL (which
	comes with v4.6 of VMS and provides a system() call) you will
	need to re-install DECWRL Modula-2 with this kit to get a new
	run-time library (without a system() call) and avoid warnings from
	the linker (duplicate symbol "system").

$ GOTO provide_lib
$!	This means that the target machine has v2.3 of VAX C RTL
$ NEW:
$!	This will rename the new version of the modula-2 run-time library (.NEW)
$!	as the current version of the library (.OLB).
$!	Use this line if the source machine has v2.2 of VAX C RTL
$!rename vmi$kwd:MODLIB.NEW vmi$kwd:MODLIB.OLB
$ TYPE SYS$INPUT

		Based on an analysis of a library listing of
	SYS$LIBRARY:VAXCRTL.OLB, this COM file has come to the conclusion
	that you have version 2.3 or later of VAX C RTL. This version
	of the VAX C RTL has a system() call. Therefore, a version of the
	Modula-2 run-time library has been provided without a system() call.

$ provide_lib:
$! This will get a copy of the correct Modula-2 run-time library
$! (with or without a system() call) and copy it to 'dmod_loc'.
$ vmi$callback provide_file dm MODLIB.OLB 'dmod_loc'
$!
$! Mail message on installation to maintainer
$!
$ DONE:
$ SET NOON
$ MAIL/SUBJ="DECWRL Modula-2 ''vsn' installed on ''node'" NL: 'maint'
$ SET ON
$!
$ IF F$VERIFY() THEN SET NOVERIFY
$ EXIT vmi$_success
$!
$err_exit:
$ s = $status
$ DELETE/NOLOG vmi$kwd:*.*;*
$ IF F$VERIFY() THEN SET NOVERIFY
$ EXIT s
