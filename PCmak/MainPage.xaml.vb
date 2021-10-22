' The Blank Page item template is documented at https://go.microsoft.com/fwlink/?LinkId=402352&clcid=0x409

Imports Windows.Storage
''' <summary>
''' An empty page that can be used on its own or navigated to within a Frame.
''' </summary>
Public NotInheritable Class MainPage
    Inherits Page
    Shared mbInSwitching As Boolean = False

#Region "Emulacja Elmak przez LIRC"
    ' to jest skopiowane z testIRwav, z ograniczeniami funkcjonalnosci

    Dim mStrm As MemoryStream       ' bez naglowka
    Dim mStrWav As MemoryStream     ' z naglowkiem
    Dim mWriter As BinaryWriter

    Private Sub ResetStream(iFreq As Integer, iChan As Integer)
        If uiGrajek.CurrentState <> MediaElementState.Closed And
                uiGrajek.CurrentState <> MediaElementState.Paused And
            uiGrajek.CurrentState <> MediaElementState.Stopped Then
            Exit Sub
        End If

        If mWriter IsNot Nothing Then mWriter.Dispose()
        If mStrm IsNot Nothing Then mStrm.Dispose()
        If mStrWav IsNot Nothing Then mStrWav.Dispose()
        mStrm = New MemoryStream
        mWriter = New BinaryWriter(mStrm)

    End Sub
    Private Sub StreamRewrite()
        mStrWav = New MemoryStream
        Dim oWriter = New BinaryWriter(mStrWav)

        ' nagłówek WAV
        Dim iFileSize As Integer ' waveSize + headerSize + formatChunkSize + headerSize + dataChunkSize
        iFileSize = 4 + 8 + 16 + 8 + mStrm.Length ' naglowki: pliku WAV, FMT chunk, DATA chunk

        oWriter.Write(&H46464952)    ' bbbb = 'RIFF'
        oWriter.Write(iFileSize)      ' bbbb
        oWriter.Write(&H45564157)    ' bbbb = 'WAVE'

        ' naglowek FMT
        ' musi przejsc przez zmienne, zeby bylo wiadomo jaki typ (ile bajtow) zapisuje
        Dim formatChunkSize As Integer = 16
        Dim formatType As Int16 = 1
        Dim tracks As Int16 = 2
        Dim bitsPerSample As Int16 = 16
        Dim frameSize As Int16 = (tracks * ((bitsPerSample + 7) / 8))
        Dim bytesPerSecond As Integer = 108000 * frameSize

        oWriter.Write(&H20746D66)    ' bbbb = 'FMT '
        oWriter.Write(formatChunkSize) ' bbbb = 16 (header len)
        oWriter.Write(formatType)    ' bb = 1: PCM
        oWriter.Write(tracks)        ' bb = 1: mono, 2: stereo, etc.
        oWriter.Write(108000) ' bbbb
        oWriter.Write(bytesPerSecond)    ' bbbb  == SampleRate * NumChannels * BitsPerSample/8
        oWriter.Write(frameSize)     ' bb  == NumChannels * BitsPerSample/8
        oWriter.Write(bitsPerSample) ' bb

        ' teraz naglowek danych
        Dim dataChunkSize As Integer = mStrm.Length

        oWriter.Write(&H61746164) '= encoding.GetBytes("data"); bbbb = 'DATA'
        oWriter.Write(dataChunkSize) ' bbbb, == NumSamples * NumChannels * BitsPerSample/8

        mStrm.WriteTo(mStrWav)

    End Sub
    Private Async Function ZapiszPrzygotowanyPlik() As Task
        Dim oFold = ApplicationData.Current.LocalFolder
        Dim oFile = Await oFold.CreateFileAsync("wavik.wav", CreationCollisionOption.ReplaceExisting)
        Dim str = Await oFile.OpenStreamForWriteAsync
        mStrWav.WriteTo(str)
        str.Dispose()

        Windows.System.Launcher.LaunchFolderAsync(oFold)
    End Function

    Private Sub PlayStream()
        mStrWav.Seek(0, SeekOrigin.Begin)

        Dim soundSource = mStrWav.AsRandomAccessStream()
        soundSource.Seek(0)
        uiGrajek.SetSource(soundSource, "audio/wav")

        uiGrajek.Play()

    End Sub

    Dim mbLED1 = True
    Dim miSrodek As Int16 = 0
    Dim miGora As Int16 = 32700
    Dim miDol As Int16 = -32700

    Private Sub Lirc_Send_Pulse(iLenUs As Integer)
        ' nosna przez iLenUs mikrosekund, albo liczba okresow

        ' Dim iLenOkres = iLenUs / 55.5333333     ' 833/55.533 da 15.0000000090036
        Dim iLenOkres As Integer = iLenUs / 27.76666      ' 833/27.766 da 30.00000720288288

        For j = 1 To iLenOkres
            If mbLED1 Then
                mWriter.Write(miGora)
                mWriter.Write(miDol)
                mWriter.Write(miGora)    ' LEDa 1
                mWriter.Write(miDol)
                mWriter.Write(miSrodek)
                mWriter.Write(miSrodek)
            Else
                mWriter.Write(miDol)
                mWriter.Write(miGora)    ' LEDa 2
                mWriter.Write(miDol)
                mWriter.Write(miGora)
                mWriter.Write(miSrodek)
                mWriter.Write(miSrodek)
            End If
            mbLED1 = Not mbLED1
        Next

        ' 1024/55.53333 = 18.43937685710545 pulse
        ' 2048/55.53333 = 36.87875371421091 space
        ' 3072/55.53333 = 55.31813057131636 space

        ' 1024/27.76666 = 36.8787603550445 pulse
        ' 2048/27.76666 = 73.75752071008901 space
        ' 3072/27.76666 = 110.6362810651335 space

    End Sub

    Private Sub Lirc_Send_Space(iLenUs As Integer)
        ' nosna przez iLenUs mikrosekund
        ' Dim iLenOkres = iLenUs / 55.5333333     ' 833/55.533 da 15.0000000090036
        Dim iLenOkres As Integer = iLenUs / 27.76666      ' 833/27.766 da 30.00000720288288

        ' to mozna dokladniej, nie co do 27.7 μs (tu może być 10 μs)

        For j = 1 To iLenOkres
            For i = 1 To 3 * 2    ' 3 sampli w 2 kanałach, odpowiednik 'pulse'
                mWriter.Write(miSrodek)
            Next
        Next

    End Sub

    Private Sub LircBit(bBit As Byte)
        If bBit Then
            Lirc_Send_Pulse(0)
            Lirc_Send_Space(833)
        Else
            Lirc_Send_Pulse(833)
            Lirc_Send_Space(0)
        End If
    End Sub

    Private Sub Lirc_Send_Data(iCode As Long, iBits As Integer)

        For i = 1 To iBits
            LircBit(iCode And &H1)
            iCode = iCode >> 1
        Next

    End Sub

    Private Sub AddIRByte(bByte As Byte)

        LircBit(0)

        Lirc_Send_Data(bByte, 8)

        LircBit(1)
        LircBit(1)
        LircBit(1)

    End Sub

    Private Sub AddIRKey(bByte As Byte)

        ' FP
        AddIRByte(&HAA)

        ' FR
        For rep = 1 To 2  ' wedle instrukcji, jedno powtorzenie
            AddIRByte(bByte)
            For i = 1 To 48
                LircBit(1)
            Next
        Next
    End Sub
#End Region

    Private Sub uiShiftToggled(sender As Object, e As RoutedEventArgs) Handles uiShift1.Toggled, uiShift2.Toggled, uiShift3.Toggled
        If mbInSwitching Then Exit Sub

        Dim oTgl = TryCast(sender, ToggleSwitch)
        Dim bBool = oTgl.IsOn

        mbInSwitching = True
        If oTgl.Name <> "uiShift1" Then uiShift1.IsOn = bBool
        If oTgl.Name <> "uiShift2" Then uiShift2.IsOn = bBool
        If oTgl.Name <> "uiShift3" Then uiShift3.IsOn = bBool
        mbInSwitching = False
    End Sub


    Private Sub uiButtonTap(sender As Object, e As TappedRoutedEventArgs) Handles uiButton1B.Tapped, uiButton01.Tapped, uiButton02.Tapped, uiButton03.Tapped, uiButton04.Tapped, uiButton05.Tapped, uiButton06.Tapped, uiButton07.Tapped, uiButton08.Tapped, uiButton09.Tapped, uiButton0B.Tapped, uiButton0A.Tapped, uiButton0C.Tapped, uiButton0D.Tapped, uiButton0E.Tapped, uiButton0F.Tapped, uiButton19.Tapped, uiButton1A.Tapped, uiButton17.Tapped, uiButton10.Tapped, uiButton11.Tapped, uiButton16.Tapped, uiButton18.Tapped, uiButton12.Tapped, uiButton15.Tapped, uiButton14.Tapped, uiButton13.Tapped, uiButton1C.Tapped, uiButton1D.Tapped, uiButton1E.Tapped, uiButton20.Tapped, uiButton21.Tapped, uiButton22.Tapped, uiButton24.Tapped, uiButton26.Tapped, uiButton27.Tapped, uiButton28.Tapped, uiButton29.Tapped, uiButton2A.Tapped, uiButton2B.Tapped
        Dim oTgl = TryCast(sender, Button)
        Dim sName = oTgl.Name
        If sName.Substring(0, 8) <> "uiButton" Then Exit Sub

        Dim iCode = Convert.ToByte(sName.Substring(8, 2), 16)
        If uiShift1.IsOn Then iCode = iCode + &H40

        ResetStream(108000, 2)
        Lirc_Send_Space(2048)

        AddIRKey(iCode)  ' next

        StreamRewrite()
        'Await ZapiszPrzygotowanyPlik()

        PlayStream()

        ' mamy kod do wyslania
    End Sub

    Private Sub uiGridLoaded(sender As Object, e As RoutedEventArgs)
        ' https://stackoverflow.com/questions/11171456/best-way-to-scroll-to-end-of-scrollviewer
        Dim scrollableHeight = uiScroll.ScrollableHeight
        If scrollableHeight > 0 Then uiScroll.ChangeView(0, scrollableHeight, Nothing)
    End Sub
End Class
