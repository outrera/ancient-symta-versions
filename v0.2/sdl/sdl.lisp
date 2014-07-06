(defpackage :sdl (:use :cl :cffi :libc :snv :gfx)
            )
(in-package :sdl)

#+win32(eval-when (:compile-toplevel :load-toplevel :execute)
         (pushnew (merge-pathnames "../bin/" (directory-namestring
                                              (or *load-truename* *default-pathname-defaults*)))
                  *foreign-library-directories*
                  :test #'equal))

;#+darwin (pushnew #p"/opt/local/lib" *foreign-library-directories*)
#|#+darwin (pushnew #p "/Users/nikita/Documents/prj/symta/dll/" *foreign-library-directories*)
(define-foreign-library sdl
  ;(:darwin (:framework "SDL"))
  ;(:darwin  (:or "libSDL.dylib"))
  ;(:darwin  (:or "/Users/nikita/Documents/prj/symta/dll/SDL"))
  ;(:darwin  (:or "/Users/nikita/Documents/prj/symta/dll/SDL.dylib"))
  (:darwin  "SDL.dylib")
  (:windows "SDL.dll")
  (:unix (:or "libSDL" "libSDL.so" "libSDL-1.2.so" "libSDL-1.2.so.0" "libSDL-1.2.so.0.7.2")))

(use-foreign-library sdl)
|#

(load-foreign-library "/Users/nikita/Documents/prj/symta/dll/cocoahelper.dylib")
;(load-foreign-library "/Users/nikita/Documents/prj/symta/dll/libSDL.dylib")
;(load-foreign-library "/Users/nikita/Documents/prj/symta/dll/libSDL_mixer.dylib")

(load-foreign-library "/opt/local/lib/libSDL.dylib")
(load-foreign-library "/opt/local/lib/libSDL_mixer.dylib")


#+darwin(defcfun "cocoahelper_init" :void)
#+darwin(cocoahelper-init)


(defmacro defenum (&body enums)	
 `(progn ,@(loop for value in enums
                 for index = 0 then (1+ index)
                 when (listp value) do (setf index (second value)
                                             value (first value))
                 collect `(defconstant ,value ,index))))

(defmacro defanonenum (&body enums)
   "Converts anonymous enums to defconstants."
  `(progn ,@(loop for value in enums
                  for index = 0 then (1+ index)
                  when (listp value) do (setf index (second value)
                                              value (first value))
                  collect `(defconstant ,value ,index))))

;;; First, set the byte-order. This is probably not needed.
(defconstant LIL_ENDIAN 1234)
(defconstant BIG_ENDIAN 4321)

;;; Set the byte order for the current CPU
#-(or little-endian PC386 X86 I386) (defconstant BYTE_ORDER BIG_ENDIAN)
#+(or little-endian PC386 X86 I386) (defconstant BYTE_ORDER LIL_ENDIAN)


(defconstant MAJOR_VERSION 1)
(defconstant MINOR_VERSION 2)
(defconstant PATCH_LEVEL 11)

(struct Version
  (major :unsigned-char)
  (minor :unsigned-char)
  (patch :unsigned-char))

(defctype Surface :pointer)
(defctype Rectangle :pointer)

(defconstant INIT_TIMER       #x00000001)
(defconstant INIT_AUDIO       #x00000010)
(defconstant INIT_VIDEO       #x00000020)
(defconstant INIT_CDROM       #x00000100)
(defconstant INIT_JOYSTICK    #x00000200)
(defconstant INIT_NOPARACHUTE #x00100000)
(defconstant INIT_EVENTTHREAD #x01000000)
(defconstant INIT_EVERYTHING  #x0000FFFF)

(defcfun ("SDL_Init" Init) :int (flags :unsigned-int))
(defcfun ("SDL_InitSubSystem" InitSubSystem) :int (flags :unsigned-int))
(defcfun ("SDL_QuitSubSystem" QuitSubSystem) :void (flags :unsigned-int))
(defcfun ("SDL_WasInit" WasInit) :unsigned-int (flags :unsigned-int))
(defcfun ("SDL_Quit" Quit) :void)
(defcfun ("SDL_Linked_Version" Linked_Version) :pointer)
(defcfun ("SDL_GetError" GetError) :string)
(defcfun ("SDL_ClearError" ClearError) :void)
(defcfun ("SDL_GetTicks" GetTicks) :int)
(defcfun ("SDL_Delay" Delay) :void (ms :int))


(defun VERSION (x)
  (with-foreign-slots ((major minor patch) x Version)
    (setf major MAJOR_VERSION
          minor MINOR_VERSION
          patch PATCH_LEVEL)))
(defun VERSIONNUM (major minor patch) (+ (* major 1000) (* minor 100) patch))
(defun COMPILEDVERSION () (VERSIONNUM MAJOR_VERSION MINOR_VERSION PATCH_LEVEL))
(defun VERSION_ATLEAST (x y z)
  (if (>= (COMPILEDVERSION) (VERSIONNUM x y z))
      1
      0))

(struct RWops
  (seek :pointer)
  (read :pointer)
  (write :pointer)
  (close :pointer)
  (type :unsigned-int)
  (hidden :pointer))

(defcunion SDL_RWops_hidden (unknown :pointer) (mem :pointer))
(struct SDL_RWops_hidden_unknown (data1 :pointer))
(struct SDL_RWops_hidden_mem (base :pointer) (here :pointer) (stop :pointer))
(defcfun ("SDL_RWFromFile" RWFromFile) :pointer (file :string) (mode :string))
(defcfun ("SDL_RWFromMem" RWFromMem) :pointer (mem :pointer) (size :int))
(defcfun ("SDL_RWFromConstMem" RWFromConstMem) :pointer (mem :pointer) (size :int))
(defcfun ("SDL_AllocRW" AllocRW) :pointer)
(defcfun ("SDL_FreeRW" FreeRW) :void (area :pointer))
(defconstant RW_SEEK_SET 0)
(defconstant RW_SEEK_CUR 1)
(defconstant RW_SEEK_END 2)
(defcfun ("SDL_ReadLE16" ReadLE16) :unsigned-short (src :pointer))
(defcfun ("SDL_ReadBE16" ReadBE16) :unsigned-short (src :pointer))
(defcfun ("SDL_ReadLE32" ReadLE32) :unsigned-int (src :pointer))
(defcfun ("SDL_ReadBE32" ReadBE32) :unsigned-int (src :pointer))
(defcfun ("SDL_ReadLE64" ReadLE64) :pointer (src :pointer))
(defcfun ("SDL_ReadBE64" ReadBE64) :pointer (src :pointer))
(defcfun ("SDL_WriteLE16" WriteLE16) :int (dst :pointer) (value :unsigned-short))
(defcfun ("SDL_WriteBE16" WriteBE16) :int (dst :pointer) (value :unsigned-short))
(defcfun ("SDL_WriteLE32" WriteLE32) :int (dst :pointer) (value :unsigned-int))
(defcfun ("SDL_WriteBE32" WriteBE32) :int (dst :pointer) (value :unsigned-int))
(defcfun ("SDL_WriteLE64" WriteLE64) :int (dst :pointer) (value :pointer))
(defcfun ("SDL_WriteBE64" SDL_WriteBE64) :int (dst :pointer) (value :pointer))




(struct AudioSpec
	(freq :int)
	(format :unsigned-short)
	(channels :unsigned-char)
	(silence :unsigned-char)
	(samples :unsigned-short)
	(padding :unsigned-short)
	(size :unsigned-int)
	(callback :pointer)
	(userdata :pointer))

(defconstant AUDIO_U8     #x0008)
(defconstant AUDIO_S8     #x8008)
(defconstant AUDIO_U16LSB #x0010)
(defconstant AUDIO_S16LSB #x8010)
(defconstant AUDIO_U16MSB #x1010)
(defconstant AUDIO_S16MSB #x9010)
(defconstant AUDIO_U16    #x0010)
(defconstant AUDIO_S16    #x8010)

(struct AudioCvt
	(needed :int)
	(src_format :unsigned-short)
	(dst_format :unsigned-short)
	(rate-incr :double)
	(buf :pointer)
	(len :int)
	(len_cvt :int)
	(len_mult :int)
	(len_ratio :double)
	(filters :pointer)
	(filter_index :int))

(defcfun ("SDL_AudioInit" AudioInit) :int (driver-name :string))
(defcfun ("SDL_AudioQuit" AudioQuit) :void)
(defcfun ("SDL_AudioDriverName" AudioDriverName) :string (namebuf :string) (maxlen :int))
(defcfun ("SDL_OpenAudio" OpenAudioSDL) :int (desired :pointer) (obtained :pointer))
(defcenum AudioStatus (:AUDIO_STOPPED 0) :AUDIO_PLAYING :AUDIO_PAUSED)
(defcfun ("SDL_GetAudioStatus" GetAudioStatus) AudioStatus)
(defcfun ("SDL_PauseAudio" PauseAudio) :void (pause_on :int))
(defcfun ("SDL_FreeWAV" FreeWAV) :void (audio_buf :pointer))
(defcfun ("SDL_BuildAudioCVT" BuildAudioCVT) :int
  (cvt :pointer)
  (src_format :unsigned-short)
  (src_channels :unsigned-char)
  (src_rate :int)
  (dst_format :unsigned-short)
  (dst_channels :unsigned-char)
  (dst_rate :int))
(defcfun ("SDL_ConvertAudio" ConvertAudio) :int
  (cvt :pointer))

(defconstant MIX_MAXVOLUME 128)

(defcfun ("SDL_MixAudio" MixAudio) :void
  (dst :pointer)
  (src :pointer)
  (len :unsigned-int)
  (volume :int))
(defcfun ("SDL_LockAudio" LockAudio) :void)
(defcfun ("SDL_UnlockAudio" UnlockAudio) :void)
(defcfun ("SDL_CloseAudio" CloseAudioSDL) :void)

#|(defcfun ("SDL_LoadWAV_RW" LoadWAV_RW) :pointer
  (src :pointer)
  (freesrc :int)
  (spec :pointer)
  (audio_buf :pointer)
  (audio_len :pointer))
(defun LoadWAV (file spec audio_buf audio_len)
  (LoadWAV_RW (RWFromFile file "rb") 1 spec audio_buf audio_len))|#



;;;; Must define the CPU byte order.
#-(or little-endian PC386 X86 I386) (defconstant AUDIO_U16SYS AUDIO_U16MSB) ;; Big Endian
#-(or little-endian PC386 X86 I386) (defconstant AUDIO_S16SYS AUDIO_S16MSB) ;; Big Endian
#+(or little-endian PC386 X86 I386) (defconstant AUDIO_U16SYS AUDIO_U16LSB) ;; Little Endian
#+(or little-endian PC386 X86 I386) (defconstant AUDIO_S16SYS AUDIO_S16LSB) ;; Little Endian




(defconstant MAX_TRACKS 99)
(defconstant AUDIO_TRACK #x00)
(defconstant DATA_TRACK #x04)
(defcenum CDstatus :CD_TRAYEMPTY :CD_STOPPED :CD_PLAYING :CD_PAUSED (:CD_ERROR -1))
(struct CDtrack
  (id :unsigned-char)
  (type :unsigned-char)
  (unused :unsigned-short)
  (length :unsigned-int)
  (offset :unsigned-int))

(struct SDL_CD
  (id :int)
  (status CDstatus)
  (numtracks :int)
  (cur_track :int)
  (cur_frame :int)
  (track :pointer))

(defcfun ("SDL_CDNumDrives" CDNumDrives) :int)
(defcfun ("SDL_CDName" CDName) :string (drive :int))
(defcfun ("SDL_CDOpen" CDOpen) :pointer (drive :int))
(defcfun ("SDL_CDStatus" CDStatus) CDstatus (cdrom :pointer))
(defcfun ("SDL_CDPlayTracks" CDPlayTracks) :int
  (cdrom :pointer)
  (start_track :int)
  (start_frame :int)
  (ntracks :int)
  (nframes :int))

(defcfun ("SDL_CDPlay" CDPlay) :int (cdrom :pointer) (start :int) (length :int))
(defcfun ("SDL_CDPause" CDPause) :int (cdrom :pointer))
(defcfun ("SDL_CDResume" CDResume) :int (cdrom :pointer))
(defcfun ("SDL_CDStop" CDStop) :int (cdrom :pointer))
(defcfun ("SDL_CDEject" CDEject) :int (cdrom :pointer))
(defcfun ("SDL_CDClose" CDClose) :void (cdrom :pointer))

(defun CD_IN_DRIVE (status)
  (if (> status 0)
      t
    nil))

(defconstant CD_FPS 75)
(defun FRAMES_TO_MSF (f)
  (values 
   (mod f CD_FPS)
   (mod (/ f CD_FPS) 60)
   (/ (/ f CD_FPS) 60)))

(defun MSF_TO_FRAMES (M S F)
  (+ 
   (* M 60 CD_FPS)
   (* S CD_FPS)
   F))




(defcfun ("SDL_NumJoysticks" NumJoysticks) :int)
(defcfun ("SDL_JoystickName" JoystickName) :string (device-index :int))
(defcfun ("SDL_JoystickOpen" JoystickOpen) :pointer (device-index :int))
(defcfun ("SDL_JoystickOpened" JoystickOpened) :int (device-index :int))
(defcfun ("SDL_JoystickIndex" JoystickIndex) :int (joystick :pointer))
(defcfun ("SDL_JoystickNumAxes" JoystickNumAxes) :int (joystick :pointer))
(defcfun ("SDL_JoystickNumBalls" JoystickNumBalls) :int (joystick :pointer))
(defcfun ("SDL_JoystickNumHats" JoystickNumHats) :int (joystick :pointer))
(defcfun ("SDL_JoystickNumButtons" JoystickNumButtons) :int (joystick :pointer))
(defcfun ("SDL_JoystickUpdate" JoystickUpdate) :void)
(defcfun ("SDL_JoystickEventState" JoystickEventState) :int (state :int))
(defcfun ("SDL_JoystickGetAxis" JoystickGetAxis) :short (joystick :pointer) (axis :int))
(defconstant HAT_CENTERED   #x00)
(defconstant HAT_UP         #x01)
(defconstant HAT_RIGHT      #x02)
(defconstant HAT_DOWN       #x04)
(defconstant HAT_LEFT       #x08)
(defconstant HAT_RIGHT_UP   (logior HAT_RIGHT HAT_UP))
(defconstant HAT_RIGHT_DOWN (logior HAT_RIGHT HAT_DOWN))
(defconstant HAT_LEFT_UP    (logior HAT_LEFT HAT_UP))
(defconstant HAT_LEFT_DOWN  (logior HAT_LEFT HAT_DOWN))
(defcfun ("SDL_JoystickGetHat" JoystickGetHat) :unsigned-char (joystick :pointer) (hat :int))
(defcfun ("SDL_JoystickGetBall" JoystickGetBall) :int
  (joystick :pointer)
  (ball :int)
  (dx :pointer)
  (dy :pointer))
(defcfun ("SDL_JoystickGetButton" JoystickGetButton) :unsigned-char (joystick :pointer) (button :int))
(defcfun ("SDL_JoystickClose" JoystickClose) :void (joystick :pointer))

(defconstant APP_MOUSE_FOCUS #x01)
(defconstant APP_INPUT_FOCUS #x02)
(defconstant APP_ACTIVE #x04)
(defcfun ("SDL_GetAppState" GetAppState) :unsigned-char)


(defcenum Key
  (:KEY_UNKNOWN 0)
  (:KEY_FIRST 0)
  (:KEY_BACKSPACE 8)
  (:KEY_TAB 9)
  (:KEY_CLEAR 12)
  (:KEY_RETURN 13)
  (:KEY_PAUSE 19)
  (:KEY_ESCAPE 27)
  (:KEY_SPACE 32)
  (:KEY_EXCLAIM 33)
  (:KEY_QUOTEDBL 34)
  (:KEY_HASH 35)
  (:KEY_DOLLAR 36)
  (:KEY_AMPERSAND 38)
  (:KEY_QUOTE 39)
  (:KEY_LEFTPAREN 40)
  (:KEY_RIGHTPAREN 41)
  (:KEY_ASTERISK 42)
  (:KEY_PLUS 43)
  (:KEY_COMMA 44)
  (:KEY_MINUS 45)
  (:KEY_PERIOD 46)
  (:KEY_SLASH 47)
  (:KEY_0 48)
  (:KEY_1 49)
  (:KEY_2 50)
  (:KEY_3 51)
  (:KEY_4 52)
  (:KEY_5 53)
  (:KEY_6 54)
  (:KEY_7 55)
  (:KEY_8 56)
  (:KEY_9 57)
  (:KEY_COLON 58)
  (:KEY_SEMICOLON 59)
  (:KEY_LESS 60)
  (:KEY_EQUALS 61)
  (:KEY_GREATER 62)
  (:KEY_QUESTION 63)
  (:KEY_AT 64)
  (:KEY_LEFTBRACKET 91)
  (:KEY_BACKSLASH 92)
  (:KEY_RIGHTBRACKET 93)
  (:KEY_CARET 94)
  (:KEY_UNDERSCORE 95)
  (:KEY_BACKQUOTE 96)
  (:KEY_a 97)
  (:KEY_b 98)
  (:KEY_c 99)
  (:KEY_d 100)
  (:KEY_e 101)
  (:KEY_f 102)
  (:KEY_g 103)
  (:KEY_h 104)
  (:KEY_i 105)
  (:KEY_j 106)
  (:KEY_k 107)
  (:KEY_l 108)
  (:KEY_m 109)
  (:KEY_n 110)
  (:KEY_o 111)
  (:KEY_p 112)
  (:KEY_q 113)
  (:KEY_r 114)
  (:KEY_s 115)
  (:KEY_t 116)
  (:KEY_u 117)
  (:KEY_v 118)
  (:KEY_w 119)
  (:KEY_x 120)
  (:KEY_y 121)
  (:KEY_z 122)
  (:KEY_DELETE 127)
  (:KEY_WORLD_0 160)
  (:KEY_WORLD_1 161)
  (:KEY_WORLD_2 162)
  (:KEY_WORLD_3 163)
  (:KEY_WORLD_4 164)
  (:KEY_WORLD_5 165)
  (:KEY_WORLD_6 166)
  (:KEY_WORLD_7 167)
  (:KEY_WORLD_8 168)
  (:KEY_WORLD_9 169)
  (:KEY_WORLD_10 170)
  (:KEY_WORLD_11 171)
  (:KEY_WORLD_12 172)
  (:KEY_WORLD_13 173)
  (:KEY_WORLD_14 174)
  (:KEY_WORLD_15 175)
  (:KEY_WORLD_16 176)
  (:KEY_WORLD_17 177)
  (:KEY_WORLD_18 178)
  (:KEY_WORLD_19 179)
  (:KEY_WORLD_20 180)
  (:KEY_WORLD_21 181)
  (:KEY_WORLD_22 182)
  (:KEY_WORLD_23 183)
  (:KEY_WORLD_24 184)
  (:KEY_WORLD_25 185)
  (:KEY_WORLD_26 186)
  (:KEY_WORLD_27 187)
  (:KEY_WORLD_28 188)
  (:KEY_WORLD_29 189)
  (:KEY_WORLD_30 190)
  (:KEY_WORLD_31 191)
  (:KEY_WORLD_32 192)
  (:KEY_WORLD_33 193)
  (:KEY_WORLD_34 194)
  (:KEY_WORLD_35 195)
  (:KEY_WORLD_36 196)
  (:KEY_WORLD_37 197)
  (:KEY_WORLD_38 198)
  (:KEY_WORLD_39 199)
  (:KEY_WORLD_40 200)
  (:KEY_WORLD_41 201)
  (:KEY_WORLD_42 202)
  (:KEY_WORLD_43 203)
  (:KEY_WORLD_44 204)
  (:KEY_WORLD_45 205)
  (:KEY_WORLD_46 206)
  (:KEY_WORLD_47 207)
  (:KEY_WORLD_48 208)
  (:KEY_WORLD_49 209)
  (:KEY_WORLD_50 210)
  (:KEY_WORLD_51 211)
  (:KEY_WORLD_52 212)
  (:KEY_WORLD_53 213)
  (:KEY_WORLD_54 214)
  (:KEY_WORLD_55 215)
  (:KEY_WORLD_56 216)
  (:KEY_WORLD_57 217)
  (:KEY_WORLD_58 218)
  (:KEY_WORLD_59 219)
  (:KEY_WORLD_60 220)
  (:KEY_WORLD_61 221)
  (:KEY_WORLD_62 222)
  (:KEY_WORLD_63 223)
  (:KEY_WORLD_64 224)
  (:KEY_WORLD_65 225)
  (:KEY_WORLD_66 226)
  (:KEY_WORLD_67 227)
  (:KEY_WORLD_68 228)
  (:KEY_WORLD_69 229)
  (:KEY_WORLD_70 230)
  (:KEY_WORLD_71 231)
  (:KEY_WORLD_72 232)
  (:KEY_WORLD_73 233)
  (:KEY_WORLD_74 234)
  (:KEY_WORLD_75 235)
  (:KEY_WORLD_76 236)
  (:KEY_WORLD_77 237)
  (:KEY_WORLD_78 238)
  (:KEY_WORLD_79 239)
  (:KEY_WORLD_80 240)
  (:KEY_WORLD_81 241)
  (:KEY_WORLD_82 242)
  (:KEY_WORLD_83 243)
  (:KEY_WORLD_84 244)
  (:KEY_WORLD_85 245)
  (:KEY_WORLD_86 246)
  (:KEY_WORLD_87 247)
  (:KEY_WORLD_88 248)
  (:KEY_WORLD_89 249)
  (:KEY_WORLD_90 250)
  (:KEY_WORLD_91 251)
  (:KEY_WORLD_92 252)
  (:KEY_WORLD_93 253)
  (:KEY_WORLD_94 254)
  (:KEY_WORLD_95 255)
  (:KEY_KP0 256)
  (:KEY_KP1 257)
  (:KEY_KP2 258)
  (:KEY_KP3 259)
  (:KEY_KP4 260)
  (:KEY_KP5 261)
  (:KEY_KP6 262)
  (:KEY_KP7 263)
  (:KEY_KP8 264)
  (:KEY_KP9 265)
  (:KEY_KP_PERIOD 266)
  (:KEY_KP_DIVIDE 267)
  (:KEY_KP_MULTIPLY 268)
  (:KEY_KP_MINUS 269)
  (:KEY_KP_PLUS 270)
  (:KEY_KP_ENTER 271)
  (:KEY_KP_EQUALS 272)
  (:KEY_UP 273)
  (:KEY_DOWN 274)
  (:KEY_RIGHT 275)
  (:KEY_LEFT 276)
  (:KEY_INSERT 277)
  (:KEY_HOME 278)
  (:KEY_END 279)
  (:KEY_PAGEUP 280)
  (:KEY_PAGEDOWN 281)
  (:KEY_F1 282)
  (:KEY_F2 283)
  (:KEY_F3 284)
  (:KEY_F4 285)
  (:KEY_F5 286)
  (:KEY_F6 287)
  (:KEY_F7 288)
  (:KEY_F8 289)
  (:KEY_F9 290)
  (:KEY_F10 291)
  (:KEY_F11 292)
  (:KEY_F12 293)
  (:KEY_F13 294)
  (:KEY_F14 295)
  (:KEY_F15 296)
  (:KEY_NUMLOCK 300)
  (:KEY_CAPSLOCK 301)
  (:KEY_SCROLLOCK 302)
  (:KEY_RSHIFT 303)
  (:KEY_LSHIFT 304)
  (:KEY_RCTRL 305)
  (:KEY_LCTRL 306)
  (:KEY_RALT 307)
  (:KEY_LALT 308)
  (:KEY_RMETA 309)
  (:KEY_LMETA 310)
  (:KEY_LSUPER 311)
  (:KEY_RSUPER 312)
  (:KEY_MODE 313)
  (:KEY_COMPOSE 314)
  (:KEY_HELP 315)
  (:KEY_PRINT 316)
  (:KEY_SYSREQ 317)
  (:KEY_BREAK 318)
  (:KEY_MENU 319)
  (:KEY_POWER 320)
  (:KEY_EURO 321)
  (:KEY_UNDO 322)
  :KEY_LAST)

;; unused, because CFFI complains
;(defcenum Mod
(defbitfield Mod
  (:KEY_MOD_LSHIFT   #x0001)
  (:KEY_MOD_RSHIFT   #x0002)
  (:KEY_MOD_LCTRL    #x0040)
  (:KEY_MOD_RCTRL    #x0080)
  (:KEY_MOD_LALT     #x0100)
  (:KEY_MOD_RALT     #x0200)
  (:KEY_MOD_LMETA    #x0400)
  (:KEY_MOD_RMETA    #x0800)
  (:KEY_MOD_NUM      #x1000)
  (:KEY_MOD_CAPS     #x2000)
  (:KEY_MOD_MODE     #x4000)
  (:KEY_MOD_RESERVED #x8000))

(struct keysym
  (scancode :unsigned-char)
  (sym Key)
  (mod :unsigned-int)
  (unicode :unsigned-short))

(defconstant ALL_HOTKEYS #xFFFFFFFF)

(defcfun ("SDL_EnableUNICODE" EnableUNICODE) :int (state :int))
(defconstant DEFAULT_REPEAT_DELAY 500)
(defconstant DEFAULT_REPEAT_INTERVAL 30)
(defcfun ("SDL_EnableKeyRepeat" EnableKeyRepeat) :int (delay :int) (interval :int))
(defcfun ("SDL_GetKeyRepeat" GetKeyRepeat) :void (delay :pointer) (interval :pointer))
(defcfun ("SDL_GetKeyState" GetKeyState) :pointer (numkeys :pointer))
(defcfun ("SDL_GetModState" GetModState) Mod)
(defcfun ("SDL_SetModState" SetModState) :void (modstate Mod))
(defcfun ("SDL_GetKeyName" GetKeyName) :string (key Key))


(struct Cursor
  (area :pointer)
  (hot_x :short)
  (hot_y :short)
  (data :pointer)
  (mask :pointer)
  (save :pointer)
  (wm_cursor :pointer))

(defcfun ("SDL_GetMouseState" GetMouseState) :unsigned-char
  (x :pointer)
  (y :pointer))
(defcfun ("SDL_GetRelativeMouseState" GetRelativeMouseState) :unsigned-char
  (x :pointer)
  (y :pointer))

(defcfun ("SDL_CreateCursor" CreateCursor) :pointer
  (data :pointer)
  (mask :pointer)
  (w :int)
  (h :int)
  (hot_x :int)
  (hot_y :int))

(defcfun ("SDL_SetCursor" SetCursor) :void (cursor :pointer))
(defcfun ("SDL_GetCursor" GetCursor) :pointer)
(defcfun ("SDL_FreeCursor" FreeCursor) :void (cursor :pointer))
(defcfun ("SDL_ShowCursor" ShowCursor) :int (toggle :int))
(defcfun ("SDL_WarpMouse" WarpMouse) :void (x :unsigned-short) (y :unsigned-short))

(defconstant BUTTON_LEFT 1)
(defconstant BUTTON_MIDDLE 2)
(defconstant BUTTON_RIGHT 3)
(defconstant BUTTON_WHEEL_UP 4)
(defconstant BUTTON_WHEEL_DOWN 5)
(defconstant RELEASED 0)
(defconstant PRESSED 1)

(defun BUTTON (X) (ash 1 (- X 1)))
(defun BUTTON_LMASK () (BUTTON BUTTON_LEFT))
(defun BUTTON_MMASK () (BUTTON BUTTON_MIDDLE))
(defun BUTTON_RMASK () (BUTTON BUTTON_RIGHT))





(defcenum EventType
  (:NO_EVENT 0)
  :ACTIVE_EVENT
  :KEY_DOWN_EVENT
  :KEY_UP_EVENT
  :MOUSE_MOTION_EVENT
  :MOUSE_BUTTON_DOWN_EVENT
  :MOUSE_BUTTON_UP_EVENT
  :JOY_AXIS_MOTION_EVENT
  :JOY_BALL_MOTION_EVENT
  :JOY_HAT_MOTION_EVENT
  :JOY_BUTTON_DOWN_EVENT
  :JOY_BUTTON_UP_EVENT
  :QUIT_EVENT
  :SYS_WM_EVENT
  :EVENT_RESERVEDA
  :EVENT_RESERVEDB
  :VIDEO_RESIZE_EVENT
  :VIDEO_EXPOSE_EVENT
  :EVENT_RESERVED2
  :EVENT_RESERVED3
  :EVENT_RESERVED4
  :EVENT_RESERVED5
  :EVENT_RESERVED6
  :EVENT_RESERVED7
  (:USER_EVENT 24)
  (:NUM_EVENTS 32))

(cl:defconstant ALL_EVENTS #xFFFFFFFF)

(struct ActiveEvent
  (type :unsigned-char)
  (gain :unsigned-char)
  (state :unsigned-char))

(struct KeyboardEvent
  (type :unsigned-char)
  (which :unsigned-char)
  (state :unsigned-char)
  (keysym keysym))

(struct MouseMotionEvent
  (type :unsigned-char)
  (which :unsigned-char)
  (state :unsigned-char)
  (x :unsigned-short)
  (y :unsigned-short)
  (xrel :short)
  (yrel :short))

(struct MouseButtonEvent
  (type :unsigned-char)
  (which :unsigned-char)
  (button :unsigned-char)
  (state :unsigned-char)
  (x :unsigned-short)
  (y :unsigned-short))

(struct JoyAxisEvent
  (type :unsigned-char)
  (which :unsigned-char)
  (axis :unsigned-char)
  (value :short))

(struct JoyBallEvent
  (type :unsigned-char)
  (which :unsigned-char)
  (ball :unsigned-char)
  (xrel :short)
  (yrel :short))

(struct JoyHatEvent
  (type :unsigned-char)
  (which :unsigned-char)
  (hat :unsigned-char)
  (value :unsigned-char))

(struct JoyButtonEvent
  (type :unsigned-char)
  (which :unsigned-char)
  (button :unsigned-char)
  (state :unsigned-char))

(struct ResizeEvent
  (type :unsigned-char)
  (w :int)
  (h :int))

(struct ExposeEvent
  (type :unsigned-char))

(struct QuitEvent
  (type :unsigned-char))

(struct UserEvent
  (type :unsigned-char)
  (code :int)
  (data1 :pointer)
  (data2 :pointer))

(struct SysWMEvent
  (type :unsigned-char)
  (msg :pointer))

(defcunion Event
  (type :unsigned-char)
  (active ActiveEvent)
  (key KeyboardEvent)
  (motion MouseMotionEvent)
  (button MouseButtonEvent)
  (jaxis JoyAxisEvent)
  (jball JoyBallEvent)
  (jhat JoyHatEvent)
  (jbutton JoyButtonEvent)
  (resize ResizeEvent)
  (expose ExposeEvent)
  (quit QuitEvent)
  (user UserEvent)
  (syswm SysWMEvent))

(defcfun ("SDL_PumpEvents" PumpEvents) :void)
(defcenum EventAction :ADD_EVENT :PEEK_EVENT :GET_EVENT)
(defcfun ("SDL_PeepEvents" PeepEvents) :int
  (events :pointer)
  (numevents :int)
  (action EventAction)
  (mask :unsigned-int))
(defcfun ("SDL_PollEvent" PollEvent) :int (event :pointer))
(defcfun ("SDL_WaitEvent" WaitEvent) :int (event :pointer))
(defcfun ("SDL_PushEvent" PushEvent) :int (event :pointer))
(defcfun ("SDL_SetEventFilter" SetEventFilter) :void (filter :pointer))
(defcfun ("SDL_GetEventFilter" GetEventFilter) :pointer)
(defcfun ("SDL_EventState" EventState) :unsigned-char (type :unsigned-char) (state :int))

(defconstant QUERY -1)
;(defconstant IGNORE 0)
(defconstant DISABLE 0)
(defconstant ENABLE 1)


(defun EVENT_MASK (X) (ash 1 X))
(defun ACTIVE_EVENT_MASK () (EVENT_MASK (foreign-enum-value 'EventType :ACTIVE_EVENT)))
(defun KEY_DOWN_MASK () (EVENT_MASK (foreign-enum-value 'EventType :KEY_DOWN_EVENT)))
(defun KEY_UP_MASK () (EVENT_MASK (foreign-enum-value 'EventType :KEY_UP_EVENT)))
(defun KEY_EVENT_MASK () (logior (EVENT_MASK (foreign-enum-value 'EventType :KEY_UP_EVENT))
                                 (EVENT_MASK (foreign-enum-value 'EventType :KEY_DOWN_EVENT))))
(defun MOUSE_MOTION_MASK () (EVENT_MASK (foreign-enum-value 'EventType :MOUSE_MOTION_EVENT)))
(defun MOUSE_BUTTON_DOWN_MASK () (EVENT_MASK (foreign-enum-value 'EventType :MOUSE_BUTTON_DOWN_EVENT)))
(defun MOUSE_BUTTON_UP_MASK () (EVENT_MASK (foreign-enum-value 'EventType :MOUSE_BUTTON_UP_EVENT)))
(defun MOUSE_EVENT_MASK () (logior (EVENT_MASK (foreign-enum-value 'EventType :MOUSE_MOTION_EVENT))
                                   (EVENT_MASK (foreign-enum-value 'EventType :MOUSE_BUTTON_DOWN_EVENT))
                                   (EVENT_MASK (foreign-enum-value 'EventType :MOUSE_BUTTON_UP_EVENT))))
(defun JOY_AXIS_MOTION_MASK () (EVENT_MASK (foreign-enum-value 'EventType :JOY_AXIS_MOTION_EVENT)))
(defun JOY_BALL_MOTION_MASK () (EVENT_MASK (foreign-enum-value 'EventType :JOY_BALL_MOTION_EVENT)))
(defun JOY_HAT_MOTION_MASK () (EVENT_MASK (foreign-enum-value 'EventType :JOY_HAT_MOTION_EVENT)))
(defun JOY_BUTTON_DOWN_MASK ()  (EVENT_MASK (foreign-enum-value 'EventType :JOY_BUTTON_DOWN_EVENT)))
(defun JOY_BUTTON_UP_MASK ()  (EVENT_MASK (foreign-enum-value 'EventType :JOY_BUTTON_UP_EVENT)))
(defun JOY_EVENT_MASK () (logior (EVENT_MASK (foreign-enum-value 'EventType :JOY_AXIS_MOTION_EVENT))
                                 (EVENT_MASK (foreign-enum-value 'EventType :JOY_BALL_MOTION_EVENT))
                                 (EVENT_MASK (foreign-enum-value 'EventType :JOY_HAT_MOTION_EVENT))
                                 (EVENT_MASK (foreign-enum-value 'EventType :JOY_BUTTON_DOWN_EVENT))
                                 (EVENT_MASK (foreign-enum-value 'EventType :JOY_BUTTON_UP_EVENT))))
(defun VIDEO_RESIZE_MASK () (EVENT_MASK (foreign-enum-value 'EventType :VIDEO_RESIZE_EVENT)))
(defun VIDEO_EXPOSE_MASK () (EVENT_MASK (foreign-enum-value 'EventType :VIDEO_EXPOSE_EVENT)))
(defun QUIT_MASK () (EVENT_MASK (foreign-enum-value 'EventType :QUIT_EVENT)))
(defun SYS_WM_EVENT_MASK ()  (EVENT_MASK (foreign-enum-value 'EventType :SYS_WM_EVENT)))




#|
(struct HWND__ (unused :int))
(struct HGLRC__ (unused :int))
(struct HDC__ (unused :int))

#+win32 (struct SysWMmsg
	(version version)
	(hwnd :pointer)
	(msg :pointer)
	(wParam :unsigned-int)
	(lParam :long))

#+win32 (struct SysWMinfo
	(version version)
	(window :pointer)
	(hglrc :pointer))

#-win32 (defcenum SYS_WM_TYPE :SYS_WM_X11)
#-win32 (defcunion SDLSysWMmsg_event (xevent :pointer))

#-win32 (struct SysWMmsg
	(version version)
	(subsystem SYS_WM_TYPE)
	(event SysWMmsg_event))

#-win32 (struct SysWMinfo_info_x11
	(display :pointer)
	(window :unsigned-long)
	(lock_func :pointer)
	(unlock_func :pointer)
	(fswindow :unsigned-long)
	(wmwindow :unsigned-long))
#-win32 (defcunion SysWMinfo_info (x11 SysWMinfo_info_x11))
#-win32 (struct SysWMinfo (version version) (subsystem SYS_WM_TYPE) (info SysWMinfo_info))

(defcfun ("SDL_GetWMInfo" GetWMInfo) :int (info :pointer))
|#



(defbitfield HardwareFlags
  (:hw_available    #x0001)
  (:wm_available    #x0002)
  (:unused_bits_1   #x0004)
  (:unused_bits_2   #x0008)
  (:unused_bits_3   #x0010)  
  (:unused_bits_4   #x0020)
  (:unused_bits_5   #x0040)
  (:unused_bits_6   #x0080)
  (:unused_bits_7   #x0100)
  (:blit_hw         #x0200)
  (:blit_hw_CC      #x0400)
  (:blit_hw_A       #x0800)
  (:blit_sw         #x1000)
  (:blit_sw_CC      #x2000)
  (:blit_sw_A       #x4000)
  (:blit_fill       #x8000)
  (:unused_bits_1_1 #x10000)
  (:unused_bits_1_2 #x20000)
  (:unused_bits_1_3 #x40000)  
  (:unused_bits_1_4 #x80000)
  (:unused_bits_1_5 #x100000)
  (:unused_bits_1_6 #x200000)
  (:unused_bits_1_7 #x400000)
  (:unused_bits_1_8 #x800000)
  (:unused_bits_2_1 #x1000000)
  (:unused_bits_2_2 #x2000000)
  (:unused_bits_2_3 #x4000000)  
  (:unused_bits_2_4 #x8000000)
  (:unused_bits_2_5 #x10000000)
  (:unused_bits_2_6 #x20000000)
  (:unused_bits_2_7 #x40000000)
  (:unused_bits_2_8 #x80000000))

(defconstant ALPHA_OPAQUE 255)
(defconstant ALPHA_TRANSPARENT 0)

(struct Rect
  (x :short)
  (y :short)
  (w :unsigned-short)
  (h :unsigned-short))

(struct Color
  (r :unsigned-char)
  (g :unsigned-char)
  (b :unsigned-char)
  (unused :unsigned-char))

(struct Palette
  (ncolors :int)
  (colors :pointer))

(struct PixelFormat
  (palette (:pointer Palette))
  (BitsPerPixel :unsigned-char)
  (BytesPerPixel :unsigned-char)
  (Rloss :unsigned-char)
  (Gloss :unsigned-char)
  (Bloss :unsigned-char)
  (Aloss :unsigned-char)
  (Rshift :unsigned-char)
  (Gshift :unsigned-char)
  (Bshift :unsigned-char)
  (Ashift :unsigned-char)
  (Rmask :unsigned-int)
  (Gmask :unsigned-int)
  (Bmask :unsigned-int)
  (Amask :unsigned-int)
  (colorkey :unsigned-int)
  (alpha :unsigned-char))

(struct Surface
  (flags :unsigned-int)
  (format (:pointer PixelFormat))
  (w :int)
  (h :int)
  (pitch :unsigned-short)
  (pixels :pointer)
  (offset :int)
  (hwdata :pointer)
  (clip_rect Rect)
  (unused1 :unsigned-int)
  (locked :unsigned-int)
  (map :pointer)
  (format_version :unsigned-int)
  (refcount :int))

(defconstant SWSURFACE   #x00000000)
(defconstant HWSURFACE   #x00000001)
(defconstant ASYNCBLIT   #x00000004)
(defconstant ANYFORMAT   #x10000000)
(defconstant HWPALETTE   #x20000000)
(defconstant DOUBLEBUF   #x40000000)
(defconstant FULLSCREEN  #x80000000)
(defconstant OPENGL      #x00000002)
(defconstant RESIZABLE   #x00000010)
(defconstant NOFRAME     #x00000020)
(defconstant HWACCEL     #x00000100)
(defconstant SRCCOLORKEY #x00001000)
(defconstant RLEACCELOK  #x00002000)
(defconstant RLEACCEL    #x00004000)
(defconstant SRCALPHA    #x00010000)
(defconstant PREALLOC    #x01000000)

(defconstant YV12_OVERLAY #x32315659)
(defconstant IYUV_OVERLAY #x56555949)
(defconstant YUY2_OVERLAY #x32595559)
(defconstant UYVY_OVERLAY #x59565955)
(defconstant YVYU_OVERLAY #x55595659)

(struct Overlay
	(format :unsigned-int)
	(w :int)
	(h :int)
	(planes :int)
	(pitches :pointer)
	(pixels :pointer)
	(hwfuncs :pointer)
	(hwdata :pointer)
	(hw_overlay :unsigned-int)
	(UnusedBits :unsigned-int))

(defcenum GLattr
	:GL_RED_SIZE
	:GL_GREEN_SIZE
	:GL_BLUE_SIZE
	:GL_ALPHA_SIZE
	:GL_BUFFER_SIZE
	:GL_DOUBLEBUFFER
	:GL_DEPTH_SIZE
	:GL_STENCIL_SIZE
	:GL_ACCUM_RED_SIZE
	:GL_ACCUM_GREEN_SIZE
	:GL_ACCUM_BLUE_SIZE
	:GL_ACCUM_ALPHA_SIZE
	:GL_STEREO
	:GL_MULTISAMPLEBUFFERS
	:GL_MULTISAMPLESAMPLES
	:GL_ACCELERATED_VISUAL
	:GL_SWAP_CONTROL)

(defconstant LOGPAL  #x01)
(defconstant PHYSPAL #x02)
(defcfun ("SDL_VideoInit" VideoInit) :int (driver_name :string) (flags :unsigned-int))
(defcfun ("SDL_VideoQuit" VideoQuit) :void)
(defcfun ("SDL_VideoDriverName" VideoDriverName) :pointer (namebuf :pointer) (maxlen :int))
(defcfun ("SDL_GetVideoSurface" GetVideoSurface) :pointer)
(defcfun ("SDL_GetVideoInfo" GetVideoInfo) :pointer)
(defcfun ("SDL_VideoModeOK" VideoModeOK) :int
  (width :int)
  (height :int)
  (bpp :int)
  (flags :unsigned-int))

(defcfun ("SDL_ListModes" ListModes) :pointer (format :pointer) (flags :unsigned-int))
(defcfun ("SDL_SetVideoMode" SetVideoMode) :pointer
  (width :int)
  (height :int)
  (bpp :int)
  (flags :unsigned-int))

(defcfun ("SDL_UpdateRects" UpdateRects) :void
  (screen :pointer)
  (numrects :int)
  (rects :pointer))

(defcfun ("SDL_UpdateRect" UpdateRect) :void
  (screen :pointer)
  (x :int)
  (y :int)
  (w :unsigned-int)
  (h :unsigned-int))

(defcfun ("SDL_Flip" Flip) :int (screen :pointer))

(defcfun ("SDL_SetGamma" SetGamma) :int
  (red :float)
  (green :float)
  (blue :float))

(defcfun ("SDL_SetGammaRamp" SetGammaRamp) :int
  (red :pointer)
  (green :pointer)
  (blue :pointer))

(defcfun ("SDL_GetGammaRamp" GetGammaRamp) :int
  (red :pointer)
  (green :pointer)
  (blue :pointer))

(defcfun ("SDL_SetColors" SetColors) :int
  (surface :pointer)
  (colors :pointer)
  (firstcolor :int)
  (ncolors :int))

(defcfun ("SDL_SetPalette" SetPalette) :int
  (surface :pointer)
  (flags :int)
  (colors :pointer)
  (firstcolor :int)
  (ncolors :int))

(defcfun ("SDL_MapRGB" MapRGB) :unsigned-int
  (format :pointer)
  (r :unsigned-char)
  (g :unsigned-char)
  (b :unsigned-char))

(defcfun ("SDL_MapRGBA" MapRGBA) :unsigned-int
  (format :pointer)
  (r :unsigned-char)
  (g :unsigned-char)
  (b :unsigned-char)
  (a :unsigned-char))

(defcfun ("SDL_GetRGB" GetRGB) :void
  (pixel :unsigned-int)
  (fmt :pointer)
  (r :pointer)
  (g :pointer)
  (b :pointer))

(defcfun ("SDL_GetRGBA" GetRGBA) :void
  (pixel :unsigned-int)
  (fmt :pointer)
  (r :pointer)
  (g :pointer)
  (b :pointer)
  (a :pointer))

(defcfun ("SDL_CreateRGBSurface" CreateRGBSurface) :pointer
  (flags :unsigned-int)
  (width :int)
  (height :int)
  (depth :int)
  (Rmask :unsigned-int)
  (Gmask :unsigned-int)
  (Bmask :unsigned-int)
  (Amask :unsigned-int))

(defcfun ("SDL_CreateRGBSurfaceFrom" CreateRGBSurfaceFrom) :pointer
  (pixels :pointer)
  (width :int)
  (height :int)
  (depth :int)
  (pitch :int)
  (Rmask :unsigned-int)
  (Gmask :unsigned-int)
  (Bmask :unsigned-int)
  (Amask :unsigned-int))

(defcfun ("SDL_FreeSurface" FreeSurface) :void (surface surface))
(defcfun ("SDL_LockSurface" LockSurface) :int (surface :pointer))
(defcfun ("SDL_UnlockSurface" UnlockSurface) :void (surface :pointer))
(defcfun ("SDL_LoadBMP_RW" LoadBMP_RW) :pointer (src :pointer) (freesrc :int))
(defcfun ("SDL_SaveBMP_RW" SaveBMP_RW) :int (surface :pointer) (dst :pointer) (freedst :int))
(defcfun ("SDL_SetColorKey" SetColorKey) :int
  (surface :pointer)
  (flag :unsigned-int)
  (key :unsigned-int))

(defcfun ("SDL_SetAlpha" SetAlpha) :int
  (surface :pointer)
  (flag :unsigned-int)
  (alpha :unsigned-char))

(defcfun ("SDL_SetClipRect" SetClipRect) :void (surface surface) (rect rectangle))
(defcfun ("SDL_GetClipRect" GetClipRect) :void (surface surface) (rect rectangle))

(defcfun ("SDL_ConvertSurface" ConvertSurface) :pointer
  (src :pointer)
  (fmt :pointer)
  (flags :unsigned-int))
(defcfun ("SDL_UpperBlit" UpperBlit) :int
  (src surface)
  (srcrect rectangle)
  (dst surface)
  (dstrect rectangle))
(defcfun ("SDL_LowerBlit" LowerBlit) :int
  (src surface)
  (srcrect rectangle)
  (dst surface)
  (dstrect rectangle))
(defcfun ("SDL_FillRect" FillRect) :int
  (dst surface)
  (dstrect rectangle)
  (color :unsigned-int))
(defcfun ("SDL_DisplayFormat" DisplayFormat) :pointer (surface :pointer))
(defcfun ("SDL_DisplayFormatAlpha" DisplayFormatAlpha) :pointer (surface :pointer))
(defcfun ("SDL_CreateYUVOverlay" CreateYUVOverlay) :pointer
  (width :int)
  (height :int)
  (format :unsigned-int)
  (display :pointer))
(defcfun ("SDL_LockYUVOverlay" LockYUVOverlay) :int (overlay :pointer))
(defcfun ("SDL_UnlockYUVOverlay" UnlockYUVOverlay) :void (overlay :pointer))
(defcfun ("SDL_DisplayYUVOverlay" DisplayYUVOverlay) :int (overlay :pointer) (dstrect :pointer))
(defcfun ("SDL_FreeYUVOverlay" FreeYUVOverlay) :void (overlay :pointer))
(defcfun ("SDL_GL_LoadLibrary" GL_LoadLibrary) :int (path :string))
(defcfun ("SDL_GL_GetProcAddress" GL_GetProcAddress) :pointer (proc :string))
(defcfun ("SDL_GL_SetAttribute" GL_SetAttribute) :int (attr GLattr) (value :int))
(defcfun ("SDL_GL_GetAttribute" GL_GetAttribute) :int (attr GLattr) (value :pointer))
(defcfun ("SDL_GL_SwapBuffers" GL_SwapBuffers) :void)
(defcfun ("SDL_GL_UpdateRects" GL_UpdateRects) :void (numrects :int) (rects :pointer))
(defcfun ("SDL_GL_Lock" GL_Lock) :void)
(defcfun ("SDL_GL_Unlock" GL_Unlock) :void)
(defcfun ("SDL_WM_SetCaption" WM_SetCaption) :void (title :string) (icon :string))
(defcfun ("SDL_WM_GetCaption" WM_GetCaption) :void (title :pointer) (icon :pointer))
(defcfun ("SDL_WM_SetIcon" WM_SetIcon) :void (icon :pointer) (mask :pointer))
(defcfun ("SDL_WM_IconifyWindow" WM_IconifyWindow) :int)
(defcfun ("SDL_WM_ToggleFullScreen" WM_ToggleFullScreen) :int (surface :pointer))
(defcenum GrabMode (:GRAB_QUERY -1) (:GRAB_OFF 0) (:GRAB_ON 1) :GRAB-FULLSCREEN)
(defcfun ("SDL_WM_GrabInput" WM_GrabInput) GrabMode (mode GrabMode))
(defcfun ("SDL_SoftStretch" SoftStretch) :int
  (src :pointer)
  (srcrect :pointer)
  (dst :pointer)
  (dstrect :pointer))
(struct VideoInfo
  (flags HardwareFlags)
  (video_mem :unsigned-int)
  (vfmt :pointer)
  (current_w :int)	;; New for SDL-1.2.11
  (current_h :int))	;; New for SDL-1.2.11
(defun LoadBMP (file) (LoadBMP_RW (RWFromFile file "rb") 1))
(defun SaveBMP (surface file) (SaveBMP_RW surface (RWFromFile file "wb") 1))
(defun BlitSurface (src srcrect dst dstrect) (UpperBlit src srcrect dst dstrect))





#|

(define-foreign-library sdl-image
  (:darwin (:framework "SDL_image"))
  ;(:darwin (:framework "libSDL_image"))
  (:windows (:or "SDL_image.dll" "SDL_image1.2.dll"))
  (:unix (:or "libSDL_image1.2" "libSDL_image-1.2.so.0")))

(use-foreign-library sdl-image)

(defcfun ("IMG_Load" IMG_Load) surface (file :string))


|#


#|
(define-foreign-library sdl-mixer
  ;(:darwin (:framework "SDL_mixer"))
  ;(:darwin (:or "libSDL_mixer.dylib"))
  ;(:darwin (:or "/opt/local/lib/libSDL_mixer.dylib"))
  ;(:darwin (:or "/opt/local/lib/libSDL_mixer-1.2.0.dylib"))
  (:darwin  (:or "/Users/nikita/Documents/prj/symta/dll/SDL.dylib"))
  (:windows "SDL_mixer.dll")
  ;(:unix (:or "libSDL_mixer" "libSDL_mixer.so" "libSDL_mixer-1.2.so" "libSDL_mixer-1.2.so.0"))
  )

(use-foreign-library sdl-mixer)
|#

#-(or little-endian PC386 X86 I386) (defconstant DEFAULT_FORMAT AUDIO_S16MSB) ; Big Endian
#+(or little-endian PC386 X86 I386) (defconstant DEFAULT_FORMAT AUDIO_S16LSB) ; Little Endian


(defconstant CHANNELS 8)
(defconstant DEFAULT_FREQUENCY 22050)
(defconstant DEFAULT_CHANNELS 2)
(defconstant MAX_VOLUME 128)

(struct chunk
  (allocated :int)
  (abuf :pointer)
  (alen :unsigned-int)
  (volume :unsigned-char))

(defbitfield InitFlags (:flac #x01) (:mod #x02) (:mp3 #x04) (:ogg #x08))
(defcenum Fading :NO_FADING :FADING_OUT :FADING_IN)
(defcenum MusicType :MUS_NONE :MUS_CMD :MUS_WAV :MUS_MOD :MUS_MID :MUS_OGG :MUS_MP3 :MUS_MP3_MAD :MUS_FLAC)

(defcfun ("Mix_OpenAudio" OpenAudio) :int
  (frequency :int)
  (format :unsigned-short)
  (chns :int)
  (chunksize :int))
(defcfun ("Mix_AllocateChannels" AllocateChannels) :int  (numchans :int))
(defcfun ("Mix_QuerySpec" QuerySpec) :int
  (frequency :pointer)
  (format :pointer)
  (chns :pointer))
(defcfun ("Mix_LoadWAV_RW" LoadWAV_RW) :pointer (src :pointer) (freesrc :int))
(defcfun ("Mix_LoadMUS" LoadMUS) :pointer (file :string))
(defcfun ("Mix_LoadMUS_RW" LoadMUS_RW) :pointer (rw :pointer))
(defcfun ("Mix_QuickLoad_WAV" QuickLoad_WAV) :pointer (mem :pointer))
(defcfun ("Mix_QuickLoad_RAW" QuickLoad_RAW) :pointer (mem :pointer) (len :unsigned-int))
(defcfun ("Mix_GetMusicType" GetMusicType) MusicType (music :pointer))
(defcfun ("Mix_SetPostMix" SetPostMix) :void (mix_func :pointer) (arg :pointer))
(defcfun ("Mix_HookMusic" HookMusic) :void (mix_func :pointer) (arg :pointer))
(defcfun ("Mix_HookMusicFinished" HookMusicFinished) :void (music_finished :pointer))
(defcfun ("Mix_GetMusicHookData" GetMusicHookData) :pointer)
(defcfun ("Mix_ChannelFinished" ChannelFinished) :void (channel_finished :pointer))
(defconstant CHANNEL_POST -2)
(defcfun ("Mix_RegisterEffect" RegisterEffect) :int
  (chan :int)
  (f :pointer)
  (d :pointer)
  (arg :pointer))
(defcfun ("Mix_UnregisterEffect" UnregisterEffect) :int (channel :int) (f :pointer))
(defcfun ("Mix_UnregisterAllEffects" UnregisterAllEffects) :int (channel :int))
(defcfun ("Mix_SetPanning" SetPanning) :int
  (channel :int)
  (left :unsigned-char)
  (right :unsigned-char))
(defcfun ("Mix_SetPosition" SetPosition) :int
  (channel :int)
  (angle :short)
  (distance :unsigned-char))
(defcfun ("Mix_SetDistance" SetDistance) :int (channel :int) (distance :unsigned-char))
(defcfun ("Mix_SetReverseStereo" SetReverseStereo) :int (channel :int) (flip :int))
(defcfun ("Mix_ReserveChannels" ReserveChannels) :int (num :int))
(defcfun ("Mix_GroupChannel" GroupChannel) :int (which :int) (tag :int))
(defcfun ("Mix_GroupChannels" GroupChannels) :int (from :int) (to :int) (tag :int))
(defcfun ("Mix_GroupAvailable" GroupAvailable) :int (tag :int))
(defcfun ("Mix_GroupCount" GroupCount) :int (tag :int))
(defcfun ("Mix_GroupOldest" GroupOldest) :int (tag :int))
(defcfun ("Mix_GroupNewer" GroupNewer) :int (tag :int))
(defcfun ("Mix_PlayChannelTimed" PlayChannelTimed) :int
  (channel :int)
  (chunk :pointer)
  (loops :int)
  (ticks :int))
(defcfun ("Mix_PlayMusic" PlayMusic) :int (music :pointer) (loops :int))
(defcfun ("Mix_FadeInMusic" FadeInMusic) :int (music :pointer) (loops :int) (ms :int))
(defcfun ("Mix_FadeInMusicPos" FadeInMusicPos) :int
  (music :pointer)
  (loops :int)
  (ms :int)
  (position :double))
(defcfun ("Mix_FadeInChannelTimed" FadeInChannelTimed) :int
  (channel :int)
  (chunk :pointer)
  (loops :int)
  (ms :int)
  (ticks :int))
(defcfun ("Mix_Volume" Volume) :int (channel :int) (volume :int))
(defcfun ("Mix_VolumeChunk" VolumeChunk) :int (chunk :pointer) (volume :int))
(defcfun ("Mix_VolumeMusic" VolumeMusic) :int (volume :int))
(defcfun ("Mix_HaltChannel" HaltChannel) :int (channel :int))
(defcfun ("Mix_HaltGroup" HaltGroup) :int (tag :int))
(defcfun ("Mix_HaltMusic" HaltMusic) :int)
(defcfun ("Mix_ExpireChannel" ExpireChannel) :int (channel :int) (ticks :int))
(defcfun ("Mix_FadeOutChannel" FadeOutChannel) :int (which :int) (ms :int))
(defcfun ("Mix_FadeOutGroup" FadeOutGroup) :int (tag :int) (ms :int))
(defcfun ("Mix_FadeOutMusic" FadeOutMusic) :int (ms :int))
(defcfun ("Mix_FadingMusic" FadingMusic) Fading)
(defcfun ("Mix_FadingChannel" FadingChannel) Fading (which :int))
(defcfun ("Mix_Pause" Pause) :void (channel :int))
(defcfun ("Mix_Resume" Resume) :void (channel :int))
(defcfun ("Mix_Paused" Paused) :int (channel :int))
(defcfun ("Mix_PauseMusic" PauseMusic) :void)
(defcfun ("Mix_ResumeMusic" ResumeMusic) :void)
(defcfun ("Mix_RewindMusic" RewindMusic) :void)
(defcfun ("Mix_PausedMusic" PausedMusic) :int)
(defcfun ("Mix_SetMusicPosition" SetMusicPosition) :int (position :double))
(defcfun ("Mix_Playing" Playing) :int (channel :int))
(defcfun ("Mix_PlayingMusic" PlayingMusic) :int)
(defcfun ("Mix_SetMusicCMD" SetMusicCMD) :int (command :string))
(defcfun ("Mix_SetSynchroValue" SetSynchroValue) :int (value :int))
(defcfun ("Mix_GetSynchroValue" GetSynchroValue) :int)
(defcfun ("Mix_GetChunk" GetChunk) :pointer (channel :int))
(defcfun ("Mix_CloseAudio" CloseAudio) :void)
(defcfun ("Mix_Linked_Version" LinkedVersion) Version)
(defcfun ("Mix_FreeChunk" FreeChunk) :void (chunk :pointer))
(defcfun ("Mix_FreeMusic" FreeMusic) :void (music :pointer))
(defun LoadWAV (file) (LoadWAV_RW (RWFromFile file "rb") 1))
(defun PlayChannel (channel chunk loops) (PlayChannelTimed channel chunk loops -1))
(defun FadeInChannel (channel chunk loops ms) (FadeInChannelTimed channel chunk loops ms -1))




(defparameter *sound-cache* nil)
(defparameter *total-channels* 16)
(defparameter *cur-channel* 0)

(defun music-load (filename)
  (let ((s (LoadMUS filename)))
    (when (/= (pointer-address s) 0)
      (tg:finalize s (lambda () (FreeMusic s)))
      s)))
(defun sound-load (filename)
  (let ((s (LoadWAV filename)))
    (when (/= (pointer-address s) 0)
      (tg:finalize s (lambda () (FreeChunk s)))
      s)))
(defun sound-set-volume (s v)
  (cond ((< v 0.0) (setf v 0.0))
        ((> v 1.0) (setf v 1.0)))
  (setf v (truncate (* v MAX_VOLUME)))
  (if (eql s :music) (VolumeMusic v) (Volume s v))
  t)
(defun sound-play (snd &key (volume 1.0) (music nil))
  (unless snd (return-from sound-play nil))
  (let* ((s (gethash snd *sound-cache*))
         (r (if music :music *cur-channel*)))
    (unless s
      (setf s (if music (music-load snd) (sound-load snd)))
      (unless s (return-from sound-play nil))
      (setf (gethash snd *sound-cache*) s))
    (sound-set-volume r volume)
    (if music (PlayMusic s 0) (PlayChannel r s 0))
    (setf *cur-channel* (mod (+ *cur-channel* 1) *total-channels*))
    r))
(defun sound-playing? (s)
  (if (eql s :music)
      (/= (PlayingMusic) 0)
      (/= (Playing s) 0)))
(defun sound-stop (s)
  (if (eql s :music)  (HaltMusic) (HaltChannel s))
  t)



(defmacro event-enum-type (enum-item-name) `(foreign-enum-value 'EventType ,enum-item-name))

(defmacro with-events (event-var &rest handlers)
  `(with-foreign-object ,`(,@event-var 'Event)
     ,`(loop while ,`(not ,`(eql ,`(PollEvent ,@event-var) 0))
          do ,`(case ,`(-> Event ,@event-var type)
                 ,@(mapcar #'(lambda (x) (list (event-enum-type (first x))
                                               (second x)))
                           handlers)))))

(defparameter *screen* nil)        ; pointer to SDL screen surface
(defparameter *input* nil)         ; buffer for input events
(defparameter *done* nil)          ; set to T to finish SDL loop


(defun mod-to-list (m)
  (mapcar (lambda (x) (string-downcase (subseq (symbol-name x) 8)))
          (foreign-bitfield-symbols 'Mod m)))

(defun process-kb-event (event down)
  (with-foreign-slots ((keysym) event KeyboardEvent)
    (with-foreign-slots ((sym mod) keysym keysym)
      (push (list (string-downcase (subseq (symbol-name sym) 4))
                  down
                  ;;(mod-to-list mod)
                  )
            *input*))))

(defun process-mice-button (event down)
  (let ((bs (vector "mice_left" "mice_middle" "mice_right" "wheel_up" "wheel_down")))
    (with-foreign-slots ((button x y) event MouseButtonEvent)
      (when (<= 1 button 5)
        (push (list (aref bs (- button 1)) down)
              *input*)))))

(defun process-mice-motion (event)
  (with-foreign-slots ((button x y) event MouseButtonEvent)
    (push (list "mice_move" (vector x y)) *input*)))


(defun convert-input (i)
  (if (atom i)
      i
      (coerce (mapcar #'convert-input i) 'vector)))

(defun process-events ()
  (with-events (event)
    (:KEY_DOWN_EVENT (process-kb-event event t))
    (:KEY_UP_EVENT (process-kb-event event nil))
    (:MOUSE_BUTTON_DOWN_EVENT (process-mice-button event t))
    (:MOUSE_BUTTON_UP_EVENT (process-mice-button event nil))
    (:MOUSE_MOTION_EVENT (process-mice-motion event))
;    (:VIDEO_RESIZE_EVENT (with-foreign-slots ((w h) event ResizeEvent)
;                           (gfx::gfx-resize *fb* w h)))
    (:QUIT_EVENT (setf *done* t)))
  (setf *input*  (convert-input (sort *input* #'string< :key #'car))))

(defmacro with-sdl (&body body)
  `(progn (when (minusp (Init (logior INIT_VIDEO INIT_TIMER)))
            (error "couldn't init sdl"))
          (setf *sound-cache* (make-hash-table :test 'equal))
          (OpenAudio 44100 AUDIO_S16SYS 2 4096)
          (setf *total-channels* (AllocateChannels 16))
          (setf *cur-channel* 0)
          ;(Volume -1 128)
          (unwind-protect (progn ,@body)
            (sound-stop :music)
            (CloseAudio)
            (Quit))))

(defun bgra-upload (d s l)
  (declare (optimize (safety 0) (speed 3) (debug 0))
           (fixnum l)
           (cptr d s))
  (let* ((i 0)
         (e (uand l #x3FFFFFF0)))
    (declare (fixnum i e))
    (while (/= i e)
      #.`(progn ,@(loop as i below 16 append
                       '((w/rgb (r g b) (mem-aref s :uint32 i)
                           (setf (mem-aref d :uint32 i) (cw 0 r g b)))
                         (incf i)))))
    (format nil "") ;; required, else sbcl would roll back loop
    (times j (- l e) (w/rgb (r g b) (mem-aref s :uint32 (f+ i j))
                       (setf (mem-aref d :uint32 (f+ i j)) (cw 0 r g b))))
    )
  nil)

(defun blit-gfx-to-surface (s g)
  ;; FIXME: add indexed and 16-bit handling
  (with-foreign-slots ((pixels w h) s Surface)
    ;; could we just termporarily replace pixels with vector-sap during resize?
#-darwin (memcpy pixels (sb-sys:vector-sap (gfx-d g)) (* w h 4))
#+darwin (bgra-upload pixels (sb-sys:vector-sap (gfx-d g)) (* w h))
    nil))



;; NOTE we should pass input to draw function, without callbacks
(defun show (handler &key (title "") (resizing nil))
  (with-sdl
    (setf *screen* nil)
    (setf *input* nil)
    (WM_SetCaption title title)
    (ShowCursor 0) ; user should use his own cursor
    (setf *done* nil)
    (block sdl-loop
      (loop while (not *done*)
         do (progn
              (process-events)
              (let ((g (funcall handler *input*)))
                (setf *input* nil)
                (unless g (return-from sdl-loop))
                (unless (gfx-p g) (error "SDL: handler returned non-gfx object"))
                (bind-struct gfx g (w h)
                  (when (or (not *screen*)
                            (/= w (-> Surface *screen* w))
                            (/= h (-> Surface *screen* h)))
                    (setf *screen* (SetVideoMode w h 32 (logior ANYFORMAT (if resizing RESIZABLE 0))))
                    (when (null-pointer-p *screen*) (error "SDL: couldn't set video mode"))))
                (blit-gfx-to-surface *screen* g)
                (Flip *screen*)))))))


(defun test-sound ()
  (with-sdl
    (sound-play "tmp/test.wav")
    (Delay 3000)))

(defun test-music ()
  (with-sdl
    (sound-play "tmp/test.ogg" :music t)
    (Delay 3000)))


(defun test-scene ()
  (let ((g (gfx 640 480)))
    (show (lambda (_)
            (gfx-clear g 0)
            (let ((c #xFFFFFF))
              (gfx-line g c 50 50 50 100)
              (gfx-line g c 50 50 100 50)
              (gfx-line g c 50 100 100 100)
              (gfx-line g c 100 50 100 100)
              (gfx-line g c 50 50 75 100)
              (gfx-line g c 100 50 75 100)
              )
            (gfx-circle g #x0000ff 200 200 50 :fill t)
            (gfx-rect g #x00ff00 200 200 80 80 :fill t)))))

(defun test-fade ()
  (let ((g (gfx 800 600)))
    (let ((fill-val 0)
          (fill-inc 4))
      (show (lambda (_)
              (cond ((<= fill-val   0) (setf fill-val   0) (setf fill-inc  1))
                    ((>= fill-val 255) (setf fill-val 255) (setf fill-inc -1)))
              (time (gfx-clear g (rgb 0 0 fill-val)))
              (incf fill-val fill-inc)
              g)))))


