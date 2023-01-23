' sokoban.pc
' (c) 1998 R.F. Lens
' info : rflens@xs4all.nl
' conv to FreeBasic Joseba Epalza <jepalza@gmail.com> 2022

' para el empleo de MULTIKEY
#include "fbgfx.bi"
#if __FB_LANG__ = "fb"
Using FB 
#EndIf


#Include "crt\stdio.bi" ' printf(), scanf(), fopen(), etc

Dim Shared as string ICO_SOKOBAN
Dim Shared as String BMP_BGROUND
Dim Shared as String BMP_BLANK
Dim Shared as String BMP_BOX
Dim Shared as String BMP_MAN
Dim Shared as String BMP_TARGET
Dim Shared as String BMP_WALL
Dim Shared as String DAT_SOKOBAN
ICO_SOKOBAN ="sokoban.ico"
BMP_BGROUND ="bground.bmp"
BMP_BLANK ="blank.bmp"
BMP_BOX ="box.bmp"
BMP_MAN ="man.bmp"
BMP_TARGET ="target.bmp"
BMP_WALL ="wall.bmp"
DAT_SOKOBAN ="sokoban.dat"

' global vars
Dim Shared As Integer campo(320) 
Dim Shared As Integer fieldorg(320) 
Dim Shared As string datafile 

Dim Shared As Integer Ptr img(6) 

Dim Shared As Integer orgmx,orgmy 
Dim Shared As Integer omx,omy 
Dim Shared As Integer mx,my 
Dim Shared As Integer level 
Dim Shared As Integer boxes,boxesdone 
Dim Shared As Integer moves 
Dim Shared As Integer undodat(100) 
Dim Shared As Integer undostart 
Dim Shared As Integer undolen 
Dim Shared As Integer undopos 
Dim Shared As string root 
Dim Shared As string extension 

Dim Shared As Integer iconsize
iconsize=48
Dim Shared As Integer tilew,tileh
tilew=20
tileh=16

Function loadres(nombre As String) As Integer Ptr
	Dim myImage As Any Ptr = ImageCreate( iconsize, iconsize )
	BLoad "icon\"+nombre, myImage
	Return myImage
End Function


Function loadmap(nombre As String) As String
	Dim mapa As String*25600 ' tamaño fijo de mapa, 25k (25600 casillas)
	Open "map\"+nombre For Input As 1
		Line Input #1,mapa
	Close 1
	Return mapa
End Function


Sub text(x As Integer, y As Integer, texto As string)
	Locate x,y
	Print texto
End Sub


Sub drawbitmap(graf As Integer,x As Integer, y As Integer)
	Put(x,y),img(graf),PSET
End Sub


Sub drawimg(x As Integer , y As Integer , imgno As Integer) 
  '0=BMP_BLANK
  '1=BMP_BGROUND 
  '2=BMP_WALL
  '3=BMP_BOX
  '4=BMP_TARGET
  '5=BMP_MAN
  drawbitmap(imgno,x*iconsize,y*iconsize+20) 
End Sub

Function strgetc(texto As String , n As Integer) As string
	Return Mid(texto,n,1)
End Function

Sub PutBackGround(x As Integer, y As Integer, a As Integer, b As integer) 
	'Line(x,y)-Step(a,b),RGB(100,0,100),bf
	For i As Integer=0 To (a/iconsize)-1
		For j As Integer=0 To (b/iconsize)-1
			drawimg((x/iconsize)+i,(y/iconsize)+j,1) ' draw only icon "1" (background)
		Next
	Next
	'Print x,y,a,b:sleep
End Sub

Sub showhelp()
	Color RGB(255,100,0)
	  text(2,40,"R:Restart Level  U:Undo last move  L:Select Level")
	Color RGB(255,255,255)
End Sub


Sub initializeall() 

  datafile=loadmap(DAT_SOKOBAN) 

  img(0)=loadres(BMP_BLANK) 
  img(1)=loadres(BMP_BGROUND) 
  img(2)=loadres(BMP_WALL) 
  img(3)=loadres(BMP_BOX) 
  img(4)=loadres(BMP_TARGET) 
  img(5)=loadres(BMP_MAN) 
  
  level=1 

  ' put background
  PutBackGround(0,0,(tilew+1)*iconsize,(tileh+1)*iconsize) 

  Color RGB(0,255,0)
  text(1,1,"Sokoban PocketC (c) 1998 R.F. Lens - PalmPC version by F.Scherz - FreeBasic by jepalza(2022)") 
  Color RGB(255,255,255)

  text(2,1, "moves") 
  text(2,20,"level") 

  showhelp()

End Sub


Sub loadlevel() 
  Dim As Integer fh 
  Dim As String linea 
  Dim As Integer i 
  Dim As Integer x,y 
  Dim As string c 
  
  linea=Mid(datafile,(320*level)-319,320)

  boxes=0 
  for x=0 To tilew-1         
    for y=0 To tileh-1       
      i=x+y*20 
      c=strgetc(linea,i) 
      if (c="0") Then 
        fieldorg(i)=0 
      ElseIf (c="1") Then
        fieldorg(i)=1
      ElseIf (c="2") Then 
        fieldorg(i)=2 
      ElseIf (c="3") Then
        fieldorg(i)=3 
        boxes+=1  
      ElseIf (c="4") Then  
        fieldorg(i)=4 
      Else 
        fieldorg(i)=0 
        orgmx=x 
        orgmy=y 
      EndIf
    Next 
  Next

End Sub

Sub startlevel() 
  Dim As Integer i 
  Dim As Integer x,y 

  for  i=0 To 319         
    campo(i)=fieldorg(i) 
  Next

  for x=1 To tilew         
    for y=0 To tileh-1         
      if(campo(x+(y*20))<>1) Then 
        drawimg(x,y,campo(x+(y*20))) 
      EndIf
    Next
  Next

  ' dibuja el jugador
  mx=orgmx 
  my=orgmy 
  drawimg(mx,my,5) 
  
  boxesdone=0 
  moves=0 
  text(2,6,":0     ")
  text(2,25,":"+Str(level)+"     ")
  undostart=0 
  undolen=0 
  undopos=0 
End Sub

function pushbox(fx As Integer , fy As Integer , dx As Integer , dy As Integer) As integer
  Dim As Integer nx,ny 
  Dim As Integer post,oldpos 

  nx=fx+dx 
  ny=fy+dy 
  post=nx+(ny*20) 
  if ((campo(post)=0) Or (campo(post)=4)) Then 
    oldpos=fx+(fy*20) 
    campo(post)=3 
    
    if (fieldorg(oldpos)=4) Then 
      campo(oldpos)=4 
    Else
      campo(oldpos)=0 
    EndIf
  
    drawimg(nx,ny,3) '3=BOX (caja)
    if ((fieldorg(oldpos)<>4) And (fieldorg(post)=4)) Then 
      boxesdone+=1  
    ElseIf ((fieldorg(oldpos)=4) And (fieldorg(post)<>4)) Then
      boxesdone-=1 
    EndIf

    return 1 
  Else
    return 0 
  EndIf
  
End Function

Sub moveman(dir_ As Integer) 
  Dim As Integer dx 
  Dim As Integer dy 
  Dim As Integer nx 
  Dim As Integer ny 
  Dim As Integer moved 
  Dim As Integer post 
  Dim As Integer res 

	Sleep(200,1)

  Select Case (dir_)  
  	case 1  
  		dx=-1
  		dy=0 
  	case 2  
  		dx=0
  		dy=-1
  	case 3  
  		dx=1
  		dy=0
  	case 4  
  		dx=0
  		dy=1
  End Select


  res=0 
  omx=mx 
  omy=my 
  nx=mx+dx 
  ny=my+dy 
  moved=0 
  post=nx+(ny*20) 
  if ((campo(post)=0) Or (campo(post)=4)) Then 
    mx=nx 
    my=ny 
    moved=1 
  ElseIf (campo(post)=3) Then
    res=pushbox(nx,ny,dx,dy)
    if (res=1) Then 
      mx=nx 
      my=ny 
      moved=1 
      dir_=dir_+4 
    EndIf
  EndIf
  

  if (moved) Then 
  	
    drawimg(omx,omy,campo(omx+omy*20)) 
    drawimg(mx,my,5) '5=MAN (jugador)
    moves+=1  
    text(2,6,":"+Str(moves))
    undodat(undopos)=dir_
    undopos+=1  
    if (undopos=100) Then 
     undopos=0 
    EndIf
  
    undolen+=1  
    if(undolen=101) Then 
      undolen=100 
      undostart+=1  
      if(undostart=101) Then 
       undostart=0 
      EndIf
    EndIf
    
  EndIf
  
End Sub

Sub movemanto(x As Integer , y As Integer) 
  Dim As Integer dx,dy 
  Dim As Integer l 

  dx= x-mx 
  dy= y-my 

  if (dx<0 And dy=0) Then 
		for l=0 To dx-1 Step -1         
		  moveman(1) 
		  sleep(400,1) 
		Next
  ElseIf (dx>0 And dy=0) Then
		for l=0 To dx-1         
		  moveman(3)
		  sleep(400,1) 
		Next
  ElseIf (dy<0 And dx=0) Then 
		for l=0 To dy-1 Step -1        
		  moveman(2) 
		  sleep(400,1) 
		Next
  ElseIf (dy>0 And dx=0) Then 
		for l=0 To dy-1        
		  moveman(4) 
		  sleep(400,1) 
		Next
  EndIf
  
End Sub

Sub restart() 
  text(2,40,"  Does you want to restart this level? (y/n)     ")
  If LCase(Left(Input(1),1))<>"y" Then showhelp():return
  showhelp()
  startlevel() 
End Sub

Sub setlevel(newlevel As Integer) 

  if (newlevel>0 And newlevel<=20) Then 
    level=newlevel 
    loadlevel() 
    startlevel() 
  EndIf
  
End Sub

Sub doundo() 
  Dim As Integer move 
  Dim As Integer push 
  Dim As Integer orgcell 
  Dim As Integer post 
  Dim As Integer dpos 
  Dim As Integer dx,dy 

	While InKey<>"":Wend

  if (undolen=0) Then 
    text(2,100,"** No moves stored **")
  Else
  	 text(2,100,"                     ")
    undopos-=1  
    if (undopos=-1) Then 
      undopos=99 
    EndIf
  
    undolen-=1  
    move=undodat(undopos) 
    push=0 
    if (move>4) Then 
      move=move-4 
      push=1 
    EndIf
  
    omx=mx 
    omy=my 
    dx=0 
    dy=0 
    if (move=1) Then 
      dpos=-1 
      dx=-1 
    ElseIf (move=2) Then
      dpos=-20
      dy=-1 
    ElseIf  (move=3) Then 
      dpos=1 
      dx=1 
    Else
      dpos=20 
      dy=1 
    EndIf
  
    ' dibuja el jugador
    mx=mx-dx 
    my=my-dy 
    drawimg(mx,my,5) '5=MAN
    
    post=omx+(omy*20) 
    if (fieldorg(post)=4) Then 
      drawimg(omx,omy,4) '4=TARGET (destino de cajas)
    Else
      drawimg(omx,omy,0) '0=BLANK (vacio)
    EndIf
  
    if (push) Then 
      campo(post)=3 
      if (fieldorg(post+dpos)=4) Then 
        campo(post+dpos)=4 
        drawimg(omx+dx,omy+dy,4) '4=TARGET (destino de cajas)
        if (fieldorg(post)<>4) Then boxesdone-=1  
      Else
        campo(post+dpos)=0
        drawimg(omx+dx,omy+dy,0) '0=BLANK (vacio)
        if (fieldorg(post)=4) Then boxesdone+=1  
      EndIf
      drawimg(omx,omy,3) '3=BOX (caja)
    EndIf
  
    moves-=1  
    text(2,6,":"+Str(moves)+"  ")
  EndIf
  
End Sub

Sub mainloop() 
  Dim As Integer done 
  Dim As Integer e 
  Dim As Integer k 
  Dim As Integer x,y 

  done=0 
  while (done=0)  

      if     MultiKey(SC_ESCAPE) Then ' salir
         done=1
      ElseIf MultiKey(SC_R) Then ' reiniciar
         restart()
      ElseIf MultiKey(SC_L) Then ' elegir nivel
         While MultiKey(SC_L):Wend
         While InKey<>"":wend
      	Color RGB(255,0,0)
      	Locate 2,100:Input "Level (1-19):",e
      	Color RGB(255,255,255)
      	Locate 2,100:Print "                 "
      	PutBackGround(0,0,(tilew+1)*iconsize,(tileh+1)*iconsize) 
         setlevel(e)
      ElseIf MultiKey(SC_U) Then ' deshacer
         While MultiKey(SC_U):Wend
         While InKey<>"":Wend
         doundo()
      EndIf
      
      ' movimientos
      if     MultiKey(SC_LEFT) Then
        moveman(1)
      ElseIf MultiKey(SC_UP) Then
        moveman(2)
      ElseIf MultiKey(SC_RIGHT) Then
        moveman(3)
      ElseIf MultiKey(SC_DOWN) Then
        moveman(4)
      EndIf
		
	if (boxes=boxesdone) Then 
	  print "Level finished, Congratulations"
	  level+=1  
	  loadlevel() 
	  startlevel() 
	EndIf

 Wend

End Sub





' -------------------------------------------------------------------------------

ScreenRes (tilew+1)*iconsize,(tileh+1)*iconsize,32

  initializeall() 
  loadlevel() 
  startlevel() 
  mainloop() 

