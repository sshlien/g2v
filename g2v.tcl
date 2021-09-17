#package provide app-gchord2voice 1.0
#!/bin/sh
# the next line restarts using wish \
exec wish "$0" "$@"
#

#g2v.tcl

# The functions in this file scan a tune in a
# abc file and produce a new tune with the 
# guitar chords (gchords) replaced with a separate
# voice where they have been replaced by notes. 
# For example the body:
#
# %%MIDI gchord fzcz
# |: cB | "F"A2F2 FAGF |
# "C7"EGB2 BdcB |"F"Acde "Dm"fagf :|
#
# is replaced with
#
# V:1
# |: cB | A2F2 FAGF |
# EGB2 BdcB |Acde fagf :|
# V:chords
# |: z2 |F,,z[F,A,C]z |
# C,,z[C,E,G,B,]z |F,,z[D,F,A,]z :|

# The guitar chords are expanded into accompaniment
# using the same convention as abc2midi; however,
# the gchord string cannot be arbitrary so that they
# can be represented by musical symbols. (See abcguide.txt
# which comes with the abcMIDI package for more details
# regarding guitar chords.)

# Method: we need the key signature, time signature,
# unit length (L:1/?) and gchord string to determine
# how to expand the guitar chords. In the music body,
# we only care about the note durations. All other
# information is ignored. For each note in the body
# we call gchord_generator, which decides whether
# to add another note or chord sequence into the
# chord voice (stored in chordvoice).
#
# For each music part (P:A, P:B etc,) we store the
# music bodytext and the expanded chords in separate
# strings. When we finish we dump everything out
# producing a new abc file.


set g2v_version 1.0
set g2v_date "March 21 2021"
set g2v_title "$g2v_version $g2v_date"

wm protocol . WM_DELETE_WINDOW {
    write_g2v_ini 
    exit
    }

proc midi_init {} {
global midi
global tcl_platform
 if {$tcl_platform(platform) == "windows"} {
  set midi(path_abc2midi) abc2midi.exe
  set midi(path_midiplayer) "C:/Program Files/Windows Media Player/wmplayer.exe"
  set midi(path_browser) "C:/Program Files (x86)/Microsoft/Edge/Application/msedge.exe"
  } else {
  set midi(path_abc2midi) abc2midi
  set midi(path_midiplayer) timidity 
  set midi(path_browser) firefox 
  }
# abc2svg settings
set midi(ps_fmt_file) ""
set midi(fmt_chk) 0
set midi(midi_chk) 0
set midi(remote) 1
set midi(jslib) "./js"
set midi(outhtml) "tune.html"
set midi(webscript) 3
}

midi_init

proc write_g2v_ini {} {
    global midi
    set outfile g2v.ini
    set handle [open $outfile w]
    #tk_messageBox -message "writing $outfile"  -type ok
    foreach item [lsort [array names midi]] {
        puts $handle "$item $midi($item)"
    }
    close $handle
}

# read all options
proc read_g2v_ini {} {
    global midi 
    set infile g2v.ini
    if {![file exist g2v.ini]} return
    set handle [open $infile r]
    #tk_messageBox -message "reading $infile"  -type ok
    while {[gets $handle line] >= 0} {
        set error_return [catch {set n [llength $line]} error_out]
        if {$error_return} continue
        set contents ""
        set param [lindex $line 0]
        for {set i 1} {$i < $n} {incr i} {
            set contents [concat $contents [lindex $line $i]]
        }
        #if param is not already a member of the midi array (set by midi_init),
        #then we ignore it. This prevents midi array filling up with obsolete
        #parameters used in older versions of the program.
        set member [array names midi $param]
        if [llength $member] { set midi($param) $contents }
    }
}

read_g2v_ini

#key.tcl

# this array is used to determine the key equivalency of the
# different modes, dorian, phrygian, etc.
	array set modeshift {
	 maj 0
	 min -3
	 m -3
	 aeo -3
	 loc -5
	 ion 0
	 dor -2
	 phr -4
	 lyd 1
	 mix -1
	}

# sequence of notes in a chromatic scale assuming sharps
	array set sharpnotes {
	 0 C
	 1 ^C
	 2 D
	 3 ^D
	 4 E
	 5 F
	 6 ^F
	 7 G
	 8 ^G
	 9 A
	10 ^A
	11 B
	}

# sequence of notes in a chromatic scale assuming flats
	array set flatnotes {
	0 C
	1 _D
	2 D
	3 _E  
	4 E
	5 F
	6 _G
	7 G
	8 _A
	9 A
	10 _B
	11 B
	}

# key2sf {keysig}
# It interprets the key signature string in the K: command
# and determines the number of sharps/flats that are placed
# on the staff. For example Bb major has two flats so sf = -2
proc key2sf {keysig} {
global modeshift
set key "FCGDAEB"
set s [regexp {([A-G]|none)(#*|b*)(.*)} $keysig match tonic accid mode]
if {$s < 1} {puts "can't understand key signature $keysig"
             return}
set sf [string first [string index $tonic 0] $key]
incr sf -1
if {$accid == "#"} {incr sf 7}
if {$accid == "b"} {incr sf -7}
set mode [string tolower $mode]
if {[info exist modeshift($mode)]} {
  set sf \ [expr $sf + $modeshift($mode)]  }
return $sf
}


# setupkey {sf}
# sets up table to convert MIDI pitches to
# notes taking into account the key signature
proc setupkey {sf} {
global basekeytable
global sharpnotes flatnotes
#if {$sf == 0} return
if {$sf >= 0} {
  for {set i 0} {$i < 12} {incr i} {
    set basekeytable($i) $sharpnotes($i)
     }
  for {set i 1} {$i <= $sf} {incr i} {
    switch $i {
     1 {set basekeytable(5) =F
        set basekeytable(6) F}
     2 {set basekeytable(0) =C
        set basekeytable(1) C}
     3 {set basekeytable(7) =G
        set basekeytable(8) G}
     4 {set basekeytable(2) =D
        set basekeytable(3) D}
     5 {set basekeytable(9) =A
        set basekeytable(10) A}
     6 {set basekeytable(4) =E
        set basekeytable(5) E}
     7 {set basekeytable(11) =B
        set basekeytable(0) B}
    }
   }    
  return
 }
if {$sf < 0} {
  for {set i 0} {$i < 12} {incr i} {
    set basekeytable($i) $flatnotes($i)
     }
  for {set i -1} {$i >= $sf} {incr i -1} {
    switch -- $i {
     -1 {set basekeytable(10) B
         set basekeytable(11) =B
         }
     -2 {set basekeytable(3) E
         set basekeytable(4) =E}
     -3 {set basekeytable(8) A
         set basekeytable(9) =A}
     -4 {set basekeytable(1) D
         set basekeytable(2) =D}      
     -5 {set basekeytable(6) G
         set basekeytable(7) =G}
     -6 {set basekeytable(11) C
         set basekeytable(0) =C}
     -7 {set basekeytable(4) F
         set basekeytable(5) =F}
     default {puts "nothing done for $i"}
   }
  }
  return
 } 
}

proc resetkeytable {} {
# copies basekeytable to keytable
global keytable basekeytable
array set keytable [array get basekeytable]
}


#converts MIDI pitch to abc music notation note
proc midi2key {midipitch} {
global keytable
set midi12 [expr $midipitch % 12]
set octave [expr $midipitch / 12]
set note $keytable($midi12)
#propagate accidentals across the bar
if {[string index $note 0] == "^"} {
   set keytable($midi12) [string index $note 1]
   set keytable([expr $midi12 -1]) "=[string index $note 1]"
   } elseif {
    [string index $note 0] == "_"} {
  set keytable($midi12) [string index $note 1]
  set keytable([expr $midi12 +1]) "=[string index $note 1]"
   } elseif {
    [string index $note 0] == "="} {
   set keytable($midi12) [string index $note 1]
   }

if {$octave == 3} {return $note,,}
if {$octave == 4} {return $note,}
return $note
}

#end of key.tcl



#tempo.tcl

set default_gchord(2/2) fzczfzcz
set default_gchord(2/4) fzczfzcz
set default_gchord(4/4) fzczfzcz
set default_gchord(3/4) fzczcz
set default_gchord(6/8) fzcfzc
set default_gchord(12/8) fzcfzcfzcfzc

# setup_meter {line}
# Handles M: command
proc setup_meter {line} {
  global default_gchord
  global gchordstring
  global barunits
  global gcstringlist
  global noteunits
  if {[string first "C|" $line] >= 0} {
    set m1 2
    set m2 2} elseif {
      [string first "C" $line] >= 0} {
    set m1 4
    set m2 4} else {
    set r [scan $line "M:%d/%d" m1 m2]}
  set barunits [expr 4*384 * $m1 /$m2]
  set meter $m1/$m2
  if {[info exist default_gchord($meter)]} {
    set gchordstring $default_gchord($meter)
    set gcstringlist [scangchordstring $gchordstring]
    }
  if {[info exists noteunits]} return
  if {[expr double($m1)/$m2] < 0.75} {
    setup_unitlength L:1/16} else {
    setup_unitlength L:1/8}
  }

#Handles L: command
proc setup_unitlength {line} {
  global noteunits
  set r [scan $line "L:%d/%d" m1 m2]
  set noteunits [expr 384 *4* $m1 /$m2]
  }

#end tempo.tcl



#gchord.tcl

# The guitar chord indications in the body of the abc file
# need to be translated in actual notes. Guitar chords
# can be quite complex and contain inversions (eg GM7/D).
# Also the notes need to be translated to the current
# key signature to avoid a proliferation of accidentals.
# (We do not account for propagation of accidentals
# inside a bar.)

# The results are stored in the lists midichord and
# gchordnotes. bassnote is the note corresponding to
# f in the gchord string.


# This array indicates the offsets in semitones of the notes
# in the different chords
array set chordnames {
	 Maj  {0 4 7}
	  m {0 3 7}
	  7 {0 4 7 10}
	  m7 {0 3 7 10}
	  maj7 {0 4 7 11}
	  M7 {0 4 7 11}
	  6 {0 4 7 9}
	  m6 {0 3 7 9}
	  aug {0 4 8}
	  plus {0 4 8}
	  aug7 {0 4 8 10}
	  dim {0 3 6}
	  dim7 {0 3 6 9}
	  9 {0 4 7 10 2}
	  m9 {0 3 7 10 2}
	  maj9 {0 4 7 11 2}
	  M9 {0 4 7 11 2}
	  11 {0 4 7 10 2 5}
	  dim9 {0 4 7 10 13}
	  sus {0 5 7}
	  sus9 {0 2 7}
	  7sus4 {0 5 7 10}
	  7sus9 {0 2 7 10}
	  5 {0 7}
	}

# expandgchord {gchord}
# The function interprets a gchord (enclosed by a pair of double quotes) in
# the music body and returns the equivalent notes forming the bass/chordal
# accompaniment in the variables bassnote and gchordnotes.
proc expandgchord {gchord} {
global notekey chordnames
global notescale
global gchordnotes
global bassnote
set chord {}
set inverpat {(.+)/(.+)}
# check for inversion
set inversion -1
if {[regexp $inverpat $gchord match a b]} {
  set gchord $a
  set invert $b
  set s [regexp {([A-G]|[a-g]|)(#*|b*)} $invert match note accid]
  if {$s <1} {puts "cannot understand inversion"}
  set n1 [string first [string index $note 0] $notekey]
  set n [expr $n1 % 7]
  set inversion [lindex $notescale $n]
  if {$accid == "#"} {incr inversion}
  if {$accid == "b"} {incr inversion -1}
  }

set s [regexp {([A-G]|[a-g]|none)(#*|b*)(.*)} $gchord match bass accid type]
if {$s < 1} {puts "can't understand chord $gchord"
             return}
set n1 [string first [string index $bass 0] $notekey]
set n [expr $n1 % 7]
if {[string compare $bass "none"] == 0} {set bass C}
set basspitch [lindex $notescale $n]
if {$accid == "#"} {incr basspitch}
if {$accid == "b"} {incr basspitch -1}
#if {[string length $sub3] > 3} {set type [string range $type 0 2 ]}
if {$type == ""} {set type Maj}

if {![info exist chordnames($type)]} {
   puts "no such chord $type"
   return
   }
set midichordnotes {}
foreach pitch $chordnames($type) {
  set midipitch [expr $pitch + $basspitch]
  lappend midichordnotes $midipitch
  }

set inchord 0
set j 0
if {$inversion != -1} {
  foreach pitch $midichordnotes {
    if {$pitch == $inversion} {set inchord $j}
    incr j
    }
 }

# Converts the gchord representation to the
# bassnote and the list of notes in the chord.
# For example Gmaj translates to bassnote G,,
# and gchordnotes {G, B, D}.
set gchordnotes {}
set j 0
set bassnote [midi2key [expr $basspitch + 36]]
foreach pitch $midichordnotes {
  set midipitch [expr $pitch +48]
  if {$j < $inchord} {incr midipitch 12}
  lappend gchordnotes [midi2key $midipitch]
  incr j
  }
}

#end gchord.tcl



#gcstring.tcl
       
proc process_MIDI_command line {
# Interpret the %%MIDI gchord command, eg. %%MIDI gchord fzcz.
global gcstringlist
global gchordunitlength expandedchords
set words [regexp -inline -all -- {\S+} $line]
set cmd [lindex $words 1]
set gchordstring [lindex $words 2]
set gcstringlist [scangchordstring $gchordstring]
setup_gchord_generator 
set gchordunitlength [return_gchord_unitlength]
set expandedchords $expandedchords$gchordunitlength\n
}


proc scangchordstring {gc} {
# Expands a gchord string (eg. fzc2fzc2 to
# f z c2 f z c2) and stores it in gcstringlist.
# Also computes the gchord string length (here 8).
global gc_string_length
set gc_string_length 0
set gclength [string length $gc]
set pat {[zcfbghijGHIJ]\d*}
set i 0
set gcstringlist {}
while {$i < $gclength} {
 set s [regexp  -indices -start $i $pat $gc r]
 if {!$s} break
 set loc1 [lindex $r 0]
 set loc2 [lindex $r 1]
# puts "[string range $gc $loc1 $loc2]  $i $loc1 $loc2"
 set gchordelem  [string range $gc $loc1 $loc2]
 lappend gcstringlist $gchordelem
 if {[string length $gchordelem] > 1} {
    set n [string index $gchordelem 1]
    } {set n 1}
 incr gc_string_length $n
 set i [expr $loc1+1]
 } 
#puts "gc_string_length=$gc_string_length"
#puts $gcstringlist
return $gcstringlist
}


proc interpret_gcstringlist {gcstringlist} {
# Converts the gchord string to an actual
# sequence of notes to place in the measure.
# This is stored in gchord_output.
global gchordnotes
global bassnote
global gchord_output
set gchord_output {}
foreach gcmd $gcstringlist {
  set g [string index $gcmd 0]
  if {[string length $g] > 0} {
     set n [string index $gcmd 1]} else {
     set n ""}
#  puts "g $g $n"
  switch $g {
    z {lappend gchord_output $gcmd}
    c {set chordstr \[
      foreach note $gchordnotes {
        set chordstr $chordstr$note
        }
      set chordstr $chordstr\]
#      puts $chordstr
      lappend gchord_output $chordstr$n
      }
    f {
      set bass $bassnote
      lappend gchord_output $bass$n
    }
    b {
      set chordstr \[$bassnote
      foreach note $gchordnotes {
        set chordstr $chordstr$note
        }
      set chordstr $chordstr\]
      lappend gchord_output $chordstr$n
      }
    g {lappend gchord_output [lindex $gchordnotes 0]$n}
    h {lappend gchord_output [lindex $gchordnotes 1]$n}
    i {lappend gchord_output [lindex $gchordnotes 2]$n}
    j {lappend gchord_output [lindex $gchordnotes 3]$n}
    G {lappend gchord_output [lindex $gchordnotes 0],$n}
    H {lappend gchord_output [lindex $gchordnotes 1],$n}
    I {lappend gchord_output [lindex $gchordnotes 2],$n}
    J {lappend gchord_output [lindex $gchordnotes 3],$n}
    default {puts "cannot recognize $g"}
   }
 }
#puts "gchord_output $gchord_output"
return $gchord_output
}
#end gcstring.tcl




#gcgen.tcl

# The notes in the expanded gchord are placed in the
# voice chord track note by note as the body of the
# abc file is scanned. This allows handling of incomplete
# measures and chord changes occurring inside a bar.
# 
# gchord_generator is called each time a note in the
# body is scanned. Anytime the gchord string (eg fczfcz)
# is changed, setup_gchord() is called. 
# 
# At the end of each bar, we call process_barline
# to ensure that the corresponding bar in the gchord
# track matches the length of the bar in the abc body.
# 
# gchordindex keeps track of the index in the
# gchord string.
# 
# gchordunit is a time unit for each gchord string
# element.

# gchordaccumulator and bar_accumulator accumulate
# the time units.


# When a measure ends we make any adjustments to the expandedchords
# so it completes the measure. We reinitialize counters.
proc process_barline {token} {
  global gchordindex
  global gchordaccumulator 
  global bar_accumulator
  global expandedchords_for_line
  if { $gchordaccumulator < $bar_accumulator} {
     set adjust [expr ($bar_accumulator - $gchordaccumulator)/192]
     set adjustelem z$adjust
     set expandedchords_for_line $expandedchords_for_line\ $adjustelem
     }
  set bar_accumulator 0
  set gchordindex 0
  set gchordaccumulator 0
  set expandedchords_for_line $expandedchords_for_line\ $token
  }

proc setup_gchord_generator {} {
 global barunits gc_string_length
 global gchordunit
 global gchordaccumulator
 global gchordindex
 if {[info exist gc_string_length]} {
   set gchordunit [expr $barunits/$gc_string_length]
# puts "gchordunit = $gchordunit gchordlenth=$gc_string_length barunits=$barunits"
   set gchordindex 0
   set gchordaccumulator 0
   } 
}

# gchord_generator is called after each note is
# scanned. The function keeps time with the
# scanned notes and issues another note in the
# gchord sequence when appropriate. 
proc gchord_generator {} {
global bar_accumulator
global gc_string_length
global gchordunit
global gchordindex
global gcstringlist
global gchordaccumulator
global gchord_output
global expandedchords_for_line
if {![info exist gcstringlist]} return
if {![info exist gchord_output]} return
if {$bar_accumulator < $gchordaccumulator} return
#puts "gchordaccumulator bar_accumulator = $gchordaccumulator $bar_accumulator"
while {$gchordaccumulator < $bar_accumulator} {
  if {$gchordindex >= [llength $gcstringlist]} return
  set gchord_output_elem [lindex $gchord_output $gchordindex]
  set gchordelem [lindex $gcstringlist $gchordindex]
  set expandedchords_for_line "$expandedchords_for_line $gchord_output_elem "
  if {[string length $gchordelem] == 1} {
    incr gchordaccumulator $gchordunit} else {
    set n [string index $gchordelem 1]
    incr gchordaccumulator [expr $gchordunit*$n]
    }
  incr gchordindex
  }
}

#end gcgen.tcl





global chordnames



proc appendtext {line} {
global fieldtext bodytext body
if {$body} {
   set bodytext $bodytext$line\n
   } else {
   set fieldtext $fieldtext$line\n
   }
}

proc find_tune {refno inputhandle} {
# To handle files which may contain a compilation of tunes
# we call this function to  position the scanner on the X:
# reference number matching refno. If refno is "none" then
# we position the scanner on the first X: field command we find.
  global fieldtext bodytext
  set bodytext ""
  while {[gets $inputhandle line] >= 0} {
    if {[string first "X:" $line] != 0} continue
      if {$refno == "none"} {
                             set fieldtext ""
                             break}
      if {[string range $line 2 end] == $refno} {
                             set fieldtext ""
                             break}
   }
   if {[eof $inputhandle]} {puts "end of file encountered" 
                            exit}

# copy tune to tunestring
  set tunestring $line\n
  while {[gets $inputhandle line] >0}  {
    set tunestring $tunestring$line\n
    }
  close $inputhandle
  return $tunestring
}



set partlabel(0)  "none"

proc process_post_P {line} {
#To handle multiple parts, we save the body and chord voice
#for each part  in separate strings which are put in
#bodyvoicelist and chordvoicelist lists respectively.
global expandedchords bodytext
global chordvoice bodyvoice
global partlabel
global npart
# the P: always precedes the body. 
if {[string length $bodytext] < 1} {
    set partlabel($npart) $line
    return}
set expandedchords [string trimright $expandedchords \n]
set chordvoice($npart) $expandedchords
set bodyvoice($npart) $bodytext
set expandedchords ""
set bodytext ""
incr npart
set partlabel($npart) $line
}

proc process_voice {line} {
global activevoice
set tline [string range $line 2 end]
set tline [split $tline]
set activevoice [lindex $tline 0]
}


proc process_line {line} {
# processes the notes in a single line of the body in sequence.
# We only care about the duration of the note and position of
# the guitar chord indications. Thus we need to recognize
# barlines eg. | || :| |: |[1 |[2
# chords eg. [CEG]
# triplets eg. (3CDD
# gchords (anything enclosed by double quotes)
# notes eg. A3/2 A/ A3 A
# Each of these entities are handled by different functions.
# When we finish the line, the string expandedchords_for_line
# contains line of music containing the gchord accompaniment.
# This string is appended to the string expandedchords.
#set barpat {\||:\|\[\d|:\||\|:|\|\[\d|\||::}
set barpat {\|\||\|[0-9]?|:\|\[\d|:\|[0-9]?|:\||\|:|\|\[\d|\||::}
set notepat {(\^*|_*|=?)([A-G]\,*|[a-g]\'*|z)(/?[0-9]*|[0-9]*/*[0-9]*)}
set gchordpat {\"[^\"]+\"}
set curlypat {\{[^\}]*\}}
set chordpat {\[[^\]\[]*\]}
set instructpat {![^!]*!}
set instructpat2 {\[I:(.*?)\]}
set tupletpat  {\(\d(\:\d)*}
set sectpat {[-,0-9]+}

global bar_accumulator
global expandedchords_for_line
global expandedchords
global bodytext
global activevoice
global preservegchord
set expandedchords_for_line ""
if {$preservegchord == 1} {
  set bodytext $bodytext$line\n
  } else {
# remove all guitar chords and print the result.
  regsub -all  {"[^"]*"} $line "" result
  set bodytext $bodytext$result\n
 }

# scan through the whole line (including gchords)
set i 0
while {$i < [string length $line]} {
# search for bar lines
 set success [regexp -indices -start $i $barpat $line location]
 if {$success} {
   set loc1  [lindex $location 0]
   set loc2  [lindex $location 1]
   if {$loc1 == $i} {
     if {$activevoice == 1} {process_barline [string range $line $loc1 $loc2]}
     set i [expr $loc2+1]
     resetkeytable
     continue}
   }

# for repeat sections
 set success [regexp -indices -start $i $sectpat $line location]
 if {$success} {
   set loc1  [lindex $location 0]
   set loc2  [lindex $location 1]
   if {$loc1 == $i} {
     set sect [string range $line $loc1 $loc2]
     set expandedchords_for_line  $expandedchords_for_line$sect
     set i [expr $loc2+1]
     continue
     }
  }



 set success [regexp -indices -start $i $gchordpat $line location]
# search for guitar chords
  if {$success} {
   set loc1  [lindex $location 0]
   set loc2  [lindex $location 1]
   if {$loc1 == $i} {process_gchord [string range $line $loc1 $loc2]
     set i [expr $loc2+1]
     continue}
     }

 set success [regexp -indices -start $i $curlypat $line location]
# search for grace note sequences
  if {$success} {
   set loc1  [lindex $location 0]
   set loc2  [lindex $location 1]
#  ignore grace notes in curly brackets
   if {$loc1 == $i} {
     set i [expr $loc2+1]
     continue}
   }

set success [regexp -indices -start $i $tupletpat $line location]
# search for triplet indication
  if {$success} {
   set loc1  [lindex $location 0]
   set loc2  [lindex $location 1]
   if {$loc1 == $i} {process_tuplet $location $line
     set i [expr $loc2+1]
     continue}
   }



 set success [regexp -indices -start $i $instructpat $line location]
# search for embedded instructions like !fff!
  if {$success} {
   set loc1  [lindex $location 0]
   set loc2  [lindex $location 1]
   if {$loc1 == $i} {
#    skip !fff! and similar instructions embedded in body
     set i [expr $loc2+1]
     continue}
   }

  set success [regexp -indices -start $i $instructpat2 $line location]
# search for embedded instructions like [I:MIDI program 3]
  if {$success} {
   set loc1  [lindex $location 0]
   set loc2  [lindex $location 1]
   if {$loc1 == $i} {
#    skip !fff! and similar instructions embedded in body
     gv_process_instruction $location $line
     set i [expr $loc2+1]
     continue}
   }
  

 set success [regexp -indices -start $i $notepat $line location]
# search for notes
 if {$success} {
   set loc1  [lindex $location 0]
   set loc2  [lindex $location 1]
   if {$loc1 == $i} {process_note [string range $line $loc1 $loc2]
     set i [expr $loc2+1]
     if {$activevoice == 1} {gchord_generator}
     continue}
   }

 set success [regexp -indices -start $i $chordpat $line location]
# search for chords
  if {$success} {
   set loc1  [lindex $location 0]
   set loc2  [lindex $location 1]
   if {$loc1 == $i} {process_chord [string range $line $loc1 $loc2]
     set i [expr $loc2+1]
     continue}
     }

 incr i
 }
set expandedchords $expandedchords$expandedchords_for_line\n
}


proc gv_process_instruction {location line} {
global gcstringlist
set instruct [string range $line [lindex $location 0] [lindex $location 1]]
set midiloc [string first MIDI $instruct]
if {$midiloc >= 0} {
   set instruct [string range $instruct 7 end-1]
   if {[string first gchord $instruct] >= 0} {
        set instruct [string range $instruct 8 end]
        set gcstringlist [scangchordstring $instruct]
        }
   }
}


# set flag to adjust the duration of the next notes enclosed in tuplet
proc process_tuplet {location line} {
global triplet_running
global tuplenotes
global nequiv
global tupletscalefactor
set tuplet [string range $line [lindex $location 0] [lindex $location 1]]
set n [scan $tuplet "(%d:%d:%d" n1 n2 n3]
#puts "tuplet = $tuplet n = $n"
set triplet_running 1
if {$n == 1} {
  set tuplesize $n1
  set tuplenotes $tuplesize
  switch $n1 {
    2 {set nequiv 3}
    3 {set nequiv 2}
    4 {set nequiv 3}
    6 {set nequiv 2}
    }
} elseif {$n == 2} {
   set tuplesize $n1
   set tuplenotes $tuplesize
   set nequiv $n2
} elseif {$n == 3} {
   set tuplesize $n1
   set nequiv $n2
   set tuplenotes $n3
}
set tupletscalefactor [expr $nequiv/double($tuplesize)]
}




# determine the duration of the note. We ignore broken
# notes (eg A > C) because the two notes usually complete
# a beat. We also do not need to pay attention to tied
# notes.
proc process_note {token} {
  global bar_accumulator
  global noteunits
  global triplet_running
  global tupletscalefactor
  global tuplenotes
  set durpatf {([0-9])\/([0-9])}
  set durpatn {[0-9]}
  set durpatd {\/([0-9])}
  set dur2 {/+}
  if {[regexp $durpatf $token match val1 val2]} {
     set increment  [expr $noteunits*$val1/$val2]
    } elseif {
  [regexp $durpatd $token val1 val2]} {
     set increment  [expr $noteunits/$val2]
     } elseif {
 [regexp $durpatn $token val]} {
     set increment  [expr $noteunits*$val]
    } elseif {
  [regexp $dur2 $token val]} {
     set increment  [expr $noteunits/2]
     } else {
  set increment $noteunits
  }
 if {$triplet_running} {
     set increment [expr round($tupletscalefactor * $increment)]
     incr triplet_running
     if {$triplet_running > $tuplenotes} {set triplet_running 0}
     }

  incr bar_accumulator $increment
  return
 }

# separate the guitar chord from the double quotes
# determine the type of chord and find the equivalent
# notes.
proc process_gchord {token} {
  global gchordstring gchord_output gcstringlist
  set token [string trimleft $token \"]
  set token [string trimright $token \"]
  expandgchord $token 
  set gchord_output [interpret_gcstringlist $gcstringlist]
  }

# We hope all the notes in the chord are of equal.
# We get the time value of the chord from the time
# value of the first note in the chord.
proc process_chord {token} {
  process_note $token
  }


# This is the function which processes the tune.
# Assume only one tune per file.
proc process_tune {tunestring} {
 global expandedchords
 global bar_accumulator
 global triplet_running
 global npart
 global body
 global debug
 global activevoice
 global nvoices

 global recover_key key_change
 global recover_meter meter_change

 set npart 0
 set nvoices 0
 set activevoice 1
 set fieldpat {^X:|^T:|^C:|^O:|^A:|^Q:|^Z:|^N:|^H:|^S:}
 set triplet_running 0
 set expandedchords ""
 set body 0
 foreach line [split $tunestring \n]  {
   if {$debug > 0} {puts ">>$line"}
   if {[string length $line] < 1} continue
   if {[string first "%%MIDI" $line] >= 0 &&
       [string first "gchord" $line] >=0} {
       process_MIDI_command $line
       appendtext $line
      } elseif {
   [string index $line 0] == "%"} {
      appendtext $line
      continue
      } elseif {
    [regexp $fieldpat $line] } {
      appendtext $line
      } elseif {
   [string first "K:" $line] == 0} {
       appendtext $line
       if {$body} {
         set expandedchords $expandedchords$line\n}
       set kfield [string range $line 2 end]
       setupkey [key2sf $kfield]
       resetkeytable
       set key_change 1
       set bar_accumulator 0
       if {!$body} {
         set body 1
         set recover_key $line
         set key_change 0
         }
      } elseif {
   [string first "M:" $line] == 0} {
       appendtext $line
       if {$body} {
         set expandedchords $expandedchords$line\n
         set meter_change 1} else {
         set recover_meter $line
         set meter_change 0}
       setup_meter $line
       setup_gchord_generator 
       set gchordunitlength [return_gchord_unitlength]
       set expandedchords $expandedchords$gchordunitlength\n
      } elseif {
   [string first "L:" $line] == 0} {
       appendtext $line
       setup_unitlength $line
      } elseif {
   [string first "P:" $line] == 0} {
       process_post_P $line
      } elseif {
   [string first "V:" $line] == 0} {
       process_voice $line
       appendtext $line
       incr nvoices
      } elseif {
   $body==1} {
       process_line $line
   } else {puts *-*-*$line }
   
 }
}



# for translating notes C,D, etc to MIDI pitches
set  notescale  {0  2  4  5  7  9  11}
set  key "cdefgab";
set notekey "CDEFGABcdefgab"





proc return_gchord_unitlength {} {
global gchordunit
if {![info exist gchordunit]} {
   return "L:1/1"} elseif {
 $gchordunit > 767} {
   return "L:1/2"} elseif {
 $gchordunit > 383} {
   return "L:1/4"} elseif {
 $gchordunit >191} {
   return "L:1/8"} elseif {
 $gchordunit > 95} {
   return "L:1/16"} else {
 return "L:1/32"}    
}
 



proc shift_all_notes {buffer shift} {
#scans line for musical notes and shifts it up or down.
#In order to continue from where we left off, we strip
#off the part of string that we have already scanned,
#and keep track of the amount we stripped in the variable
#offset plus any expansion or contraction of note (eg. b to c').
  set note {[A-G]\,*|[a-g]\'*}
  # decorations regex explained:
  # (?n) - honor lines:  ^ and $
  # (\![^\!]*\!)           - !decoration!
  # (^[ \t]*%.*$)          - comment lines (lines beginning with %)
  # (^[ \t]*[A-Za-z]:.*$)  - keyword lines
  # (\![^\!]*\!)           - !decoration!
  # (\[[ \t]*\"[^\"]*\"))  - part strings "part1"   (probably should be any strings, including guitar chords)
  #   (\"[^\"]*\")	   - guitar chords
  #   (\[.:[^]]*\])       - inline field command like [K: Gm]
  set decoration {(?n)((\![^\!]*\!)|(^[ \t]*%.*$)|(^[ \t]*[A-Za-z]:.*$)|(\[[ \t]*\"[^\"]*\"))|(\"[^\"]*\")|(\[.:[^]]*\])}
  set tmp $buffer
  set success 1
  set offset 0
  while {$success} {
    set success [regexp -indices $note $tmp match]
    if {!$success} break
    set skip [regexp -indices $decoration $tmp match_skip]
    # if the decoration comes before the note then skip the decoration
    if {$skip && [lindex $match_skip 0] < [lindex $match 0]} {
      set pos2 [lindex $match_skip 1]
      set offset [expr $offset + $pos2 + 1]
      set tmp [string range $buffer $offset end]
    } else {
      set pos1 [lindex $match 0]
      set pos2 [lindex $match 1]
      set key [string range $tmp  $pos1 $pos2]
      set newkey [shift_note $key $shift]
#      puts "$key $newkey"
      set tmp [string range $tmp [expr $pos2 +1] end]
      set apos1 [expr $pos1 + $offset]
      set apos2 [expr $pos2 + $offset]
      set buffer [string replace $buffer $apos1 $apos2 $newkey]
      set offset [expr $offset + $pos1 + [string length $newkey]]
#      puts "$offset $pos1 $pos2"
    }
  }
  return $buffer
}
  



proc shift_note {note shift} {
# shift note by a given musical interval,up or down.
# 1 or -1 is second
# 2 or -2 is third...
global notekey
set n [string first [string index $note 0] $notekey]
set m [expr [string length $note] - 1]
if {$n < 7 && $m >0} {set m [expr -$m] }
set n [expr $n + $shift]
if {$n>13} {set n [expr $n - 7]
            incr m
  } elseif {$n<0} {set n [expr $n + 7]
           incr m -1
  }
if {$n < 7 && $m>0} {
            set n [expr $n + 7]
            incr m -1
   } elseif {$n > 6 && $m<0} {
            set n [expr $n - 7]
            incr m
   }
set result [string index $notekey $n]
if {$m > 0} {set suffix [string repeat "'" $m]
             set result $result$suffix
 } elseif {$m < 0} { set m [expr -$m]
                     set suffix [string repeat "," $m]
                     set result $result$suffix
                    }
return $result
}



set abctypes {{{abc files} {*.abc}}}

proc gchordgui {} {
global inputfile xref debug
global outputfile
global xref
global abctypes
set outputfile tmp.abc
frame  .gui
frame  .gui.1
button .gui.1.0 -text input  -command {set inputfile [tk_getOpenFile -filetypes $abctypes]
                                       title_index $inputfile}
entry  .gui.1.1 -width 40 -textvariable inputfile
button .gui.1.4 -text output -command {set outputfile [tk_getSaveFile]}
entry  .gui.1.5 -width 40 -textvariable outputfile 
pack .gui
pack .gui.1
pack .gui.1.0 .gui.1.1  .gui.1.4 .gui.1.5 -side left
frame .gui.2
pack .gui.2
button .gui.2.expand -text expand  -command g2v
menubutton .gui.2.save -text save -relief raised -menu .gui.2.save.s
set p .gui.2.save.s
menu $p -tearoff 0
$p add command -label "abc file"  -command {Text_Dump $outputfile}
$p add command -label "midi file" -command save_midi_file

button .gui.2.play -text play -command play_file
button .gui.2.display -text display -command display_text
menubutton .gui.2.6 -text configure -relief raised -menu .gui.2.6.c
set p .gui.2.6.c
menu $p -tearoff 0
$p add command -label "general configuration" -command configuration_gui
$p add command -label "configure midi arrangement" -command make_voice_canvas


pack  .gui.2.expand .gui.2.save .gui.2.display .gui.2.play  .gui.2.6 -side left
bind .gui.1.1 <Return> {$abctxtw delete 1.0 end
			title_index $inputfile}
}




proc arranger_gui {} {
global abctxtw
global activevoice
panedwindow .pane
frame .pane.tframe
set abctxtw .pane.tframe.t
text $abctxtw -wrap none \
     -yscrollcommand {.pane.tframe.ysbar set} \
     -xscrollcommand {.pane.tframe.xsbar set} 
 scrollbar .pane.tframe.ysbar -orient vertical -command {$abctxtw yview}
 scrollbar .pane.tframe.xsbar -orient horizontal -command {$abctxtw xview}
label .mesg  -width 60 -text "gchord to voices"
pack .mesg -side top -anchor w -fill x
pack .pane.tframe.ysbar -side right -fill y 
pack $abctxtw -fill both -expand 1
pack .pane.tframe.xsbar -side bottom -fill x 
$abctxtw configure -undo 1 
$abctxtw edit modified 0
focus 
frame .pane.toolbox
listbox .pane.toolbox.list -yscrollcommand {.pane.toolbox.tsbar set} -width 28
scrollbar .pane.toolbox.tsbar -command {.pane.toolbox.list yview}
.pane add .pane.toolbox .pane.tframe
pack .pane.toolbox.list .pane.toolbox.tsbar -side left -fill y -expand y
 pack .pane -fill both -expand 1
}

set nrepeats 3
set randomize 1

proc configuration_gui {} {
global preservegchord 
global tempo
global midi
if {[winfo exist .cfg]} return
toplevel .cfg
checkbutton .cfg.pre -text "preserve gchords" -variable preservegchord
button .cfg.playerbut -text "midiplayer path"  -command {set midi(path_midiplayer) [tk_getOpenFile]}
button .cfg.abc2midibut -text "abc2midi path"  -command {set midi(path_abc2midi) [tk_getOpenFile]}
button .cfg.browserbut -text "browser path"  -command {set midi(path_browser) [tk_getOpenFile]}
entry  .cfg.playerent -width 40 -textvariable midi(path_midiplayer)
entry  .cfg.abc2midient -width 40 -textvariable midi(path_abc2midi)
entry  .cfg.browserent -width 40 -textvariable midi(path_browser)
label .cfg.nrepeatstxt -text "number of repeats"
scale .cfg.nrepeats -length 200  -from 1 -to 10 -variable nrepeats  -showvalue 1 \
  -orient horizontal
label .cfg.tempotxt -text "beats per minute"
scale .cfg.tempo -length 200  -from 60 -to 240 -variable tempo  -showvalue 1 \
  -orient horizontal
checkbutton .cfg.ran -text "random arrangement" -variable randomize
entry .cfg.arrange -width 40 -textvariable midi_descriptors
grid .cfg.abc2midibut .cfg.abc2midient
grid .cfg.playerbut .cfg.playerent
grid .cfg.browserbut .cfg.browserent
grid .cfg.tempotxt .cfg.tempo
grid .cfg.nrepeatstxt .cfg.nrepeats
grid .cfg.pre .cfg.ran
grid .cfg.arrange -columnspan 2 
}


proc title_index {abcfile} {
global fileseek
global nrepeats
global outputfile
set titlehandle [open $abcfile r]
set pat {[0-9]+}
set srch X
.pane.toolbox.list delete 0 end
set filepos 0
set i 0
while {[gets $titlehandle line] >=0 } {
  set lastfilepos $filepos
  set filepos [tell $titlehandle]
  switch -- $srch {
  X {if {[string compare -length 2 $line "X:"] == 0} {
     regexp $pat $line number
     set fileseek($number) $lastfilepos
     set srch T
     }
   }
  T {if {[string compare -length 2 $line "T:"] == 0} {
     set name [string range $line 2 end]
     set name [string trim $name]
     set listentry [format "%4s  %s" $number $name]
     incr i
     .pane.toolbox.list insert end $listentry
     set srch X
     }
   }
 }
}
close $titlehandle
bind .pane.toolbox.list <Button> {set selection [.pane.toolbox.list index @%x,%y]
   set item [.pane.toolbox.list get $selection]
   set xref [lindex $item 0]
   set title [lindex $item 1]
   set u _
   for {set i 2} {$i < [llength $item]} {incr i} {
      set title $title$u[lindex $item $i]
       }
   set title [string trimright $title]
   if {[string length $title] <1} {set title notitle}
# remove any apostrophe's
    set pat \'
    regsub -all $pat $title "" result
    set outputfile $result
    load_abc_file $inputfile $xref 
    random_selection $nrepeats 
    }
if {$i == 1} {load_abc_file $abcfile $number
              random_selection $nrepeats} 
}


proc load_abc_file {abcfile number} {
global abctxtw fileseek
global xref
set xref $number
 $abctxtw delete 1.0 end
 set edit_handle [open $abcfile r]
 if {[eof $edit_handle] !=1} {
 set loc  $fileseek($number)
 seek  $edit_handle $loc
 gets $edit_handle line
 $abctxtw insert end $line\n 
 }
 while {[string length $line] > 0} {
	gets $edit_handle line
	$abctxtw insert end $line\n 
        } 
 close $edit_handle
}





proc process_part {line} {
}



arranger_gui 
#load_abc_file outcl.abc


# MIDI voice descriptors
# comment program velocity octave

#        name, midi program, midi velocity, octave shift
set midi1(1) {french_horn 60 60 0}
set midi1(2) {flute 73 95 2}
set midi1(3) {clav 7 60 -1}
set midi1(4) {koto 107 70 0}

set midi2(1) {violin 40 100 0}
set midi2(2) {acoustic_bass 32 40 2}
set midi2(3) {synth_voice 90 40 -1}
set midi2(4) {oboe 68 80 0}

set midi3(1) {Violin 40 70 0}
set midi3(2) {Fiddle 110 70 1}
set midi3(3) {Cello 42 70 -1}
set midi3(4) {tympani 48 80 0}

set midi4(1) {clarinet 71 75 0}
set midi4(2) {Soprano_Sax 64 79 0}
set midi4(3) {Baritone_Sax 67 79 -1}
set midi4(4) {Dulcimer 15 86 0}

set midi5(1) {Celesta 8 103 0}
set midi5(2) {Tenor_Sax 66 75 0}
set midi5(3) {Pizzicato_Strings 45 60 -1}
set midi5(4) {FX_4_(atmosphere) 99 70 0}

set midi6(1) {violin 40 100 0}
set midi6(2) {Reed_Organ 20 71 0}
set midi6(3) {synth_voice 90 67 -1}
set midi6(4) {oboe 68 80 1}

set midi7(1) {Electric_Bass_(finger) 33 75 0}
set midi7(2) {Acoustic_Guitar_(nylon) 24 70 -1}
set midi7(3) {Electric_Guitar_(jazz) 26 70 1}
set midi7(4) {tympani 48 80 0}

set midi8(1) {clarinet 71 70 0}
set midi8(2) {piccolo 72 100 0}
set midi8(3) {clav 7 50 -1}
set midi8(4) {woodblock 115 70 0}

set midi9(1) {Bassoon 70 91 0}
set midi9(2) {flute 73 60 1}
set midi9(3) {Pizzicato_Strings 45 60 -1}
set midi9(4) {Tuba 58 70 0}

set midi10(1) {Percussive_Organ 17 87 0}
set midi10(2) {Electric_Bass_(finger) 33 87 0}
set midi10(3) {Skakuhachi 77 83 -1}
set midi10(4) {oboe 68 80 0}

set midi11(1) {Muted_Trumpet 59 60 0}
set midi11(2) {Acoustic_Grand 0 70 -1}
set midi11(3) {Bright_Acoustic 1 70 1}
set midi11(4) {tympani 48 80 0}


proc random_selection {n} {
global midi_descriptors
set selection {midi1 midi2 midi3 midi4 midi5 midi6 midi7 midi8 midi9 midi10 midi11}
set midi_descriptors {}
if {$n > 12} {set n 12}
for {set i 0} {$i <$n} {incr i} {
  set nsize [llength $selection]
  set j [expr int(rand()*$nsize)]
  lappend midi_descriptors [lindex $selection $j]
  set selection [lreplace $selection $j $j] 
  }
}


proc export_midi_commands {descriptor} {
global abctxtw
set prog [lindex $descriptor 1]
set vel [lindex $descriptor 2]
set buf "%%MIDI program $prog\n"
$abctxtw insert end $buf
set buf "%%MIDI control 7 $vel\n"
$abctxtw insert end $buf
}

proc export_arrangement {} {
global midi1 midi2 midi3 midi4 midi5
global midi6 midi7 midi8 midi9
global midi10 midi11
global midiplayer_path

set exhandle [open "g2varr.tcl" "w"]
puts $exhandle "#        name, midi program, midi velocity, octave shift"
for {set i 1} {$i < 12} {incr i} {
 for {set j 1} {$j < 5} {incr j} { 
   set v midi$i\(
   set v $v$j\)
   puts $exhandle "set $v \{[lindex [array get midi$i $j] 1]\}"
   }
  puts $exhandle ""
  }
puts $exhandle "set midiplayer_path \{$midiplayer_path\}"
close $exhandle
 }

proc import_arrangement {} {
global midi1 midi2 midi3 midi4 midi5
global midi6 midi7 midi8 midi9
global midi10 midi11

source g2varr.tcl
}
# MAIN program
set xref "none"
set debug 0
set preservegchord 0
gchordgui


proc return_midi_cmds {marray i} {
upvar $marray mx
return $mx($i)
}

set tempo 160

proc g2v {} {
global inputfile xref fieldtext
global bodyvoice
global abctxtw
global npart
global partlabel
global nrepeats
global chordvoice
global midi_descriptors
global midi1 midi2 midi3 midi4 midi5
global midi6 midi7 midi8 midi9 midi10
global midi11
global tempo

global recover_key key_change
global recover_meter meter_change

set inputhandle [open $inputfile r]
set tunestring [find_tune $xref $inputhandle]
process_tune $tunestring
process_post_P "none"

# output
set tempostring "Q:1/4=$tempo\n"
set vc "V:4 clef=bass\n"
set output_text $fieldtext
$abctxtw delete 1.0 end
$abctxtw insert end $fieldtext fieldata 
$abctxtw insert end $tempostring fieldata 
set clef8 " clef=treble+8"
for {set i 0} {$i <$npart} {incr i} {
  if {$partlabel($i) != "none"} {
	$abctxtw insert end $partlabel($i)\n part
     }
  for {set repeat 0} {$repeat < $nrepeats} {incr repeat} {
  set midirepeat [lindex $midi_descriptors $repeat] 
  $abctxtw insert end "%\n% ---- repeat $repeat ----$midirepeat\n"
  for {set n 1} {$n < 4} {incr n} {
    set midicmds [return_midi_cmds $midirepeat $n]
    set oct [lindex $midicmds 3]
    set vcmd "V:$n"
    if {$oct == 2} {set vcmd $vcmd$clef8}
    $abctxtw insert end $vcmd\n 
    if {$meter_change && $repeat > 0} {
      $abctxtw insert end $recover_meter\n}
    if {$key_change && $repeat > 0} {
      $abctxtw insert end $recover_key\n}
    export_midi_commands $midicmds
    if {$oct > 0} {
      set transposedvoice [shift_all_notes $bodyvoice($i) 7]
      $abctxtw insert end $transposedvoice 
      } elseif {$oct < 0} {
      set transposedvoice [shift_all_notes $bodyvoice($i) -7]
      $abctxtw insert end $transposedvoice 
      } else {
      $abctxtw insert end $bodyvoice($i) 
      }
    }
  $abctxtw insert end $vc 
  if {$meter_change && $repeat > 0} {
     $abctxtw insert end $recover_meter\n}
  if {$key_change && $repeat > 0} {
     $abctxtw insert end $recover_key\n}
  export_midi_commands [return_midi_cmds $midirepeat 4]
  $abctxtw insert end $chordvoice($i)\n 
  }
 }
}

proc Text_Dump {outputfile {start 1.0} {end end}} {
global abctxtw
  if {[file extension $outputfile] == ".abc"} {
    set outhandle [open $outputfile w] } else {
    set outhandle [open $outputfile.abc w]}

  foreach {key value index} [$abctxtw dump $start $end] {
    if {$key == "text"} {
      puts -nonewline $outhandle $value}
    }
  close $outhandle
  .mesg configure -text "saved $outputfile" -fg black
}

proc play_file {} {
global midi 
Text_Dump tmp.abc
if {[file exist $midi(path_abc2midi)]} {
  set cmd "exec [list $midi(path_abc2midi)] [list tmp.abc -o tmp.mid]"
  eval $cmd
  } else {
  .mesg configure -text "I cannot find the abc2midi executable" -fg red
  return}
  set cmd  "exec [list $midi(path_midiplayer)] [list [pwd]/tmp.mid] &"
  update
  if {[catch {eval $cmd} errormsg]} {
     .mesg configure -text $errormsg -fg red
     } else {
     .mesg configure -text "playing tmp.mid" -fg black
    }
}



proc save_midi_file {} {
global midi
global outputfile
Text_Dump tmp.abc
if {[file extension $outputfile] == ".abc"} {
  set outputfile [file rootname $outputfile]}
if {[file exist $midi(path_abc2midi)]} {
  set cmd "exec [list $midi(path_abc2midi)] [list tmp.abc -o $outputfile.mid]"}
  if {[catch {eval $cmd} errormsg]} {
     .mesg configure -text "unable to save $outputfile.mid" -fg red
     } else {
     .mesg configure -text "saved $outputfile" -fg black
     }
}

#end g2v.tcl


#source voices.tcl

proc voice_button {num X Y} {
global window voicenum
set window .voice.canvas.f.prog$num
set voicenum $num
my_popup $X $Y}



proc my_popup {rootx rooty} {
global m 

   if {![winfo exists .patchmap]} {
      set instrum_family {piano "chrom percussion" organ guitar bass \
      strings ensemble brass reed pipe "synth lead" "synth pad" \
      "synth effects" ethnic percussive "sound effects"}
      set w .patchmap
      menu $w -tearoff 0
      set i 1
      foreach class $instrum_family {
        $w add cascade  -label $class -menu $w.$i
        set w2 .patchmap.$i
        menu $w2 -tearoff 0
        set j 0
        foreach inst $m($i) {
          $w2 add radiobutton -label $inst \
              -command "program_select  $i $j "
          incr j
          }
        incr i
      }
   }
   tk_popup .patchmap $rootx $rooty 
}

proc program_select {p1 p2} {
global prog voicenum window
global midi1 midi2 midi3 midi4 midi5
global midi6 midi7 midi8 midi9
global midi10 midi11
global ar
set midarr midi$ar
set midipar  [lindex [array get $midarr $voicenum] 1]
puts $midipar
set prog [expr ($p1-1)*8 + $p2]
set progname [.patchmap.$p1 entrycget $p2 -label]
set newprogname [string map {" " _} $progname]
$window configure -text $progname
set midipar [lreplace $midipar 0 0 $newprogname]
set midipar [lreplace $midipar 1 1 $prog]
puts $midipar
array set $midarr [list $voicenum $midipar]
}


set m(1) {"Acoustic Grand" "Bright Acoustic" "Electric Grand" \
"Honky-Tonk" "Electric Piano 1" "Electric Piano 2" Harpsichord \
Clav }

set m(2) {Celesta Glockenspiel "Music Box" Vibraphone Marimba \
Xylophone "Tubular Bells" Dulcimer}

set m(3) {"Drawbar Organ" "Percussive Organ" "Rock Organ" \
"Church Organ" "Reed Organ" Accordian Harmonica "Tango Accordian"}

set m(4) { "Acoustic Guitar (nylon)" "Acoustic Guitar (steel)" \
"Electric Guitar (jazz)" "Electric Guitar (clean)" \
 "Electric Guitar (muted)" "Overdriven Guitar" \
"Distortion Guitar" "Guitar Harmonics"} 

set m(5) {"Acoustic Bass" "Electric Bass (finger)" \
"Electric Bass (pick)" "Fretless Bass" "Slap Bass 1" \
"Slap Bass 2" "Synth Bass 1" "Synth Bass 2" }

set m(6) { Violin Viola Cello Contrabass "Tremolo Strings" \
"Pizzicato Strings" "Orchestral Strings" Timpani }

set m(7) { "String Ensemble 1" "String Ensemble 2" "SynthStrings 1" \
"SynthStrings 2" "Choir Aahs" "Voice Oohs" "Synth Voice" "Orchestra Hit" }

set m(8) { Trumpet Trombone Tuba "Muted Trumpet" "French Horn" \
"Brass Section" "SynthBrass 1" "SynthBrass 2"}

set m(9) { "Soprano Sax" "Alto Sax" "Tenor Sax" "Baritone Sax" \
 Oboe "English Horn" Bassoon Clarinet }

set m(10) { Piccolo Flute Recorder "Pan Flute" "Blown Bottle" \
Skakuhachi Whistle Ocarina }

set m(11) { "Lead 1 (square)" "Lead 2 (sawtooth)" "Lead 3 (calliope)" \
"Lead 4 (chiff)" "Lead 5 (charang)" "Lead 6 (voice)" \
"Lead 7 (fifths)" "Lead 8 (bass+lead)"} 

set m(12) { "Pad 1 (new age)" "Pad 2 (warm)" "Pad 3 (polysynth)" \
"Pad 4 (choir)" "Pad 5 (bowed)" "Pad 6 (metallic)" "Pad 7 (halo)" \
"Pad 8 (sweep)" }

set m(13) { "FX 1 (rain)" "(soundtrack)" "FX 3 (crystal)" \
"FX 4 (atmosphere)" "FX 5 (brightness)" "FX 6 (goblins)" \
"FX 7 (echoes)" "FX 8 (sci-fi)" }

set m(14) { Sitar Banjo Shamisen Koto Kalimba \
 Bagpipe Fiddle Shanai} 

set m(15) { "Tinkle Bell" Agogo "Steel Drums" Woodblock \
"Taiko Drum" "Melodic Tom" "Synth Drum" "Reverse Cymbal" }

set m(16) { "Guitar Fret Noise" "Breath Noise" Seashore \
"Bird Tweet" "Telephone ring" Helicopter Applause Gunshot }


proc make_voice_canvas {} {
global ar
global midi1 midi2 midi3 midi4 midi5
global midi6 midi7 midi8 midi9
global midi10 midi11
global m vel octave
if {[winfo exist .voice]} return
toplevel .voice
canvas .voice.canvas -width 320 -height 230 \
   -yscrollcommand [list .voice.yscroll set]
scrollbar .voice.yscroll -orient vertical \
   -command [list .voice.canvas yview]
pack  .voice.yscroll -side right -fill y
pack  .voice.canvas -side left -fill both -expand true

set w [frame .voice.canvas.f -bd 0]
.voice.canvas create window 0 0 -anchor nw -window $w

button $w.export -text export -command export_arrangement
button $w.import -text import -command import_arrangement
grid  $w.export -row 0 -column 1 
grid  $w.import -row 0 -column 2 
set ar 1
label $w.arrtext -text arrangement
scale $w.arrscale -length 150  -from 1 -to 11 -variable ar -showvalue 1 \
  -orient horizontal -command reconfigure_voice_canvas
grid $w.arrtext -row 1 -columnspan 2 -column 0
grid $w.arrscale -row 1 -columnspan 2 -column 2

label $w.head0 -text V: 
label $w.head1 -text program 
label $w.head2 -text level 
label $w.head3 -text octave 

grid $w.head0 $w.head1 $w.head2 $w.head3
for {set i 1} {$i <5} {incr i} {
   set midarr midi$ar
   set midipar  [lindex [array get $midarr $i] 1]
   label $w.lab$i -text $i 
   set prog [lindex $midipar 1]
   set vel($i) [lindex $midipar 2]
   set octave($i) [lindex $midipar 3]
   set i1 [expr int(1 + $prog/8)] 
   set i2 [expr $prog % 8 ]
   button $w.prog$i -text [lindex $m($i1) $i2]   -width 15 -pady 1
   eval {bind $w.prog$i <Button> [list voice_button $i %X %Y]}
   scale $w.vel$i -from 0 -to 127 -length 64  \
    -width 8 -orient horizontal  -showvalue true \
    -variable vel($i) -command "update_velocity  midi$ar $i"
   scale $w.oct$i -from -1 -to 2 -length 64  \
    -width 8 -orient horizontal  -showvalue true \
    -variable octave($i) -command "update_octave midi$ar $i"
   grid $w.lab$i $w.prog$i $w.vel$i $w.oct$i
}
}


proc reconfigure_voice_canvas {ar} {
global midi1 midi2 midi3 midi4 midi5
global midi6 midi7 midi8 midi9
global midi10 midi11
global m vel octave
set midarr midi$ar
set w .voice.canvas.f
for {set i 1} {$i <5} {incr i} {
   set midipar  [lindex [array get $midarr $i] 1]
   set prog [lindex $midipar 1]
   set vel($i) [lindex $midipar 2]
   set octave($i) [lindex $midipar 3]
   set i1 [expr int(1 + $prog/8)] 
   set i2 [expr $prog % 8 ]
   $w.prog$i configure -text [lindex $m($i1) $i2]
   $w.vel$i configure -command "update_velocity  midi$ar $i"
   $w.oct$i configure -command "update_octave midi$ar $i"
   update
  }
}

proc update_velocity {par i vel} {
  upvar $par midipar
  set midipar($i) [lreplace $midipar($i) 2 2 $vel]
  }

proc update_octave {par i oct} {
  upvar $par midipar
  set midipar($i) [lreplace $midipar($i) 3 3 $oct]
  }

#end of source voices.tcl

# reset random number generator
expr srand([clock clicks])


# abc2svg functions
#


proc display_text {} {
global midi
copytohtml
set cmd "exec [list $midi(path_browser)] file:[list [pwd]/$midi(outhtml)] &"
catch {eval $cmd} exec_out
set exec_out "$cmd\n$exec_out"
}

set urlpointer(0) "https://boubounet.fr/misc/"
set urlpointer(1) "http://moinejf.free.fr/js/"
set urlpointer(2) $midi(jslib)/



proc make_js_script_list {url abcweb} {
set scriptlist "        <meta http-equiv=\"Content-Type\" content=\"text/html; charset=UTF-8\"/>\n"
set styleblock "
        <style type=\"text/css\">
        svg {display:block}
        @media print{body{margin:0;padding:0;border:0}.nop{display:none}}
        </style>"
set w(0) "abc2svg-1.js"
set w(1) "snd-1.js"
set w(2) "follow-1.js"
set tail "\"></script>\n"
append scriptlist "     <script src=\"$url$w(0)$tail"
append scriptlist "     <script src=\"$url$abcweb$tail"
append scriptlist "     <script src=\"$url$w(1)$tail"
append scriptlist "     <script src=\"$url$w(2)$tail"
append scriptlist $styleblock
return $scriptlist
}


proc copytohtml {} {
global midi
global urlpointer
global abctxtw

set html_preamble "<!DOCTYPE HTML>
<html>
<head>
"

 switch $midi(webscript) {
     1 {set abcweb abcweb-1.js}
     2 {set abcweb abcweb1-1.js}
     3 {set abcweb abcweb2-1.js}
     }

set remote_svg_script [make_js_script_list $urlpointer(1) $abcweb]
set local_svg_script  [make_js_script_list $urlpointer(2) $abcweb]

set preface $html_preamble
  if {$midi(remote)} {
    set preface $preface$remote_svg_script
    } else {
    set preface $preface$local_svg_script
    }

set wholefile [$abctxtw get 0.0 end] 



set outhandle [open $midi(outhtml) w]
puts $outhandle $preface
if {$midi(webscript) == 3} {
  puts $outhandle  "</head>\n<body>\n<script type=\"text/vnd.abc\" class=\"abc\">"
  } else {
  puts $outhandle  "</head>\n<body>"
  }


if {$midi(fmt_chk) && [file exist $midi(ps_fmt_file)]} {
  set inhandle [open $midi(ps_fmt_file)]
  set fmt [read $inhandle]
  close $inhandle
  puts $outhandle $fmt
  }
puts $outhandle $wholefile
   switch $midi(webscript) {
       1 {puts $outhandle "\n</body>\n</html>\n"}
       2 {puts $outhandle "\n-->\n</body>\n</html>\n"}
       3 {puts $outhandle  "</script>\n</body>\n</html>\n"}
       }

close $outhandle
}

wm title . $g2v_title
