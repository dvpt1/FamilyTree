*---------------------------------------------------------*
procedure DrawTitle
private old_color
save scre
old_color=setcolor(ct)
Tenj( 4, 8,22,74)
@  3,6 say '   靈컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�   '
@  4,6 say ' 郞胱�                                                       �誥拷 '
@  5,6 say ' � 훤�                                                       훤� � '
@  6,6 say ' �                � � � � � � � H � �  � � � � �                 � '
@  7,6 say ' �                                                               � '
@  8,6 say ' �                        궏褻⑨ 11.98                           � '
@  9,6 say ' �                                                               � '
@ 10,6 say ' �                 �™�褻ぎ� 뤲젪� (c) 1993-1996                 � '
@ 11,6 say ' �                                                               � '
@ 12,6 say ' �                       꽟ⓥ黍� 뒶�赤��                         � '
@ 13,6 say ' �                                                               � '
@ 14,6 say ' �                                                               � '
@ 15,6 say ' �              떒璵�㎤ⅸ 췅 將� 칛�즤젹с �∥젮젰�              � '
@ 16,6 say ' � 닾�:        '+substr(name_firm,1,50)+'� '
@ 17,6 say ' � 럓짛��쭬與�:'+substr(name_firm,51,50)+'� '
@ 18,6 say ' �                                                               � '
@ 19,6 say ' � �敲                                                       �敲 � '
@ 20,6 say ' 突胱�                                                       훤胱� '
@ 21,6 say '   聃컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�   '
ch=inkey(0)
setcolor(old_color)
rest scre
return
