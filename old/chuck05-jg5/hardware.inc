%macro DELAY 0
	out 0xe1, al
%endmacro

; DMA mode register (write only)
DMA_MODE equ 0xb
; and values for it - select channel 2, in either direction.
DMA_IN	equ 0x16
DMA_OUT	equ 0x1a

; floppy drive select register (read/write)
FDC_DRIVES	equ 0x3f2
; and values for it; select drive 0, turn motor on or off.
MOTOR0_ON	equ 0x10
MOTOR0_OFF	equ 0x00
DRIVE0		equ 0x0c

; floppy drive main status register (read only)
; when the high bit is set, we're clear to send a command.
FDC_STATUS	equ 0x3f4

; floppy drive command/status register (read/write)
FDC_DATA	equ 0x3f5


; floppy drive command values and lengths
; (note that length includes the command byte)

CMD_SET_TIMINGS	equ 0x03
LEN_SET_TIMINGS	equ 3
; set-timings has its own arguments:
; assuming the data rate is set to 500Kbps,
; the two argument bytes are (values in milliseconds):
; (16-STEP_RATE)<<4 | UNLOAD_TIME/16
; (LOAD_TIME/2)<<1  | NO_DMA

; read/write use the longest set of arguments (see 'command' in boot.asm).
; the rest of the commands use only the first couple of those arguments.
CMD_READ	equ 0xe6
CMD_WRITE	equ 0xc5
LEN_RD_WR	equ 9
CMD_CALIBRATE	equ 0x07	; calibrate drive after spinning it up
LEN_CALIBRATE	equ 2
CMD_SEEK	equ 0x0f	; seek to cylinder
LEN_SEEK	equ 3
; after doing a rewind or seek, we should wait until SENSE_INT
; indicates an active interrupt to tell us that the command is finished.
; If we don't issue a SENSE_INT command, the drive "will continue to be
; BUSY, which may affect the operation of next command."
CMD_SENSE_INT	equ 0x08
LEN_SENSE_INT	equ 1

NO_INTERRUPT	equ 0x80

; vim:ts=8:sw=8
