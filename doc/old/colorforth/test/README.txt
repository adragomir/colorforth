Test results:

2006-11-27-1243Z: make BXDEBUG=1 newbxnewcf

sba 11114984
trace on
s 100

(0).[11114984] [0x00001f60] 0008:00001f60 (unk. ctxt): rep movsd dword ptr es:[e
di], dword ptr ds:[esi] ; f3a5
(0).[11114986] [0x00001f60] 0008:00001f60 (unk. ctxt): rep movsd dword ptr es:[e
di], dword ptr ds:[esi] ; f3a5
(0).[11114988] [0x00001f60] 0008:00001f60 (unk. ctxt): rep movsd dword ptr es:[e
di], dword ptr ds:[esi] ; f3a5
(0).[11114990] [0x00001f60] 0008:00001f60 (unk. ctxt): add byte ptr ds:[eax], al
 ; 0000
(0).[11114991] [0x00001f62] 0008:00001f62 (unk. ctxt): add byte ptr ds:[eax], al
 ; 0000
(0).[11114992] [0x00001f64] 0008:00001f64 (unk. ctxt): jmp .+0xffffe36b (0x00000
2d4) ; e96be3ffff
(0).[11114993] [0x000002d4] 0008:000002d4 (unk. ctxt): push ds
 ; 1e
(0).[11114994] [0x000002d5] 0008:000002d5 (unk. ctxt): add byte ptr ds:[esi], bl
 ; 001e
(0).[11114995] [0x000002d7] 0008:000002d7 (unk. ctxt): add byte ptr ds:[esi], bl
 ; 001e
(0).[11114996] [0x000002d9] 0008:000002d9 (unk. ctxt): add byte ptr ds:[esi], bl
 ; 001e
(0).[11114997] [0x000002db] 0008:000002db (unk. ctxt): add byte ptr ds:[esi], bl
 ; 001e
(0).[11114998] [0x000002dd] 0008:000002dd (unk. ctxt): add byte ptr ds:[esi], bl

Looks like memory got overwritten during the 'rep movsd'.

Aha! Found out why. 0x1f55 is 'mov edi, displ'. displ has an unusable value
since the AGP detection failed.
