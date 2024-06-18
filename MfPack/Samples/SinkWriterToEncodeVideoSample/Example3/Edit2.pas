//  OnAudioSampleRequested()
//
//  Called when audio device fires m_SampleReadyEvent.
//
function TLoopbackCapture.OnAudioSampleRequested(): HResult;
var
  hr: HResult;
  br: BOOL;
  FramesAvailable: UINT32;
  pData: pByte;
  pBufferData: pByte;
  dwCaptureFlags: DWord;
  cbBytesToCapture: DWord;
  dwBytesWritten: DWord;
  NumBytesWritten: INT64;

label
  leave;

begin

  hr := S_OK;
  NumBytesWritten := 0;
  pData := nil;

  // If this flag is set, we have already queued up the async call to finialize the WAV header.
  // So we don't want to grab or write any more data that would possibly give us an invalid size.
  if not (m_DeviceState = Capturing) then
    goto leave;

  // A word on why we have a loop here;
  // Suppose it has been 10 milliseconds or so since the last time
  // this routine was invoked, and that we're capturing 48000 samples per second.
  // The audio engine can be reasonably expected to have accumulated about that much
  // audio data - that is, about 480 samples.
  //
  // However, the audio engine is free to accumulate this in various ways:
  // a. as a single packet of 480 samples, OR
  // b. as a packet of 80 samples plus a packet of 400 samples, OR
  // c. as 48 packets of 10 samples each.
  //
  // In particular, there is no guarantee that this routine will be
  // run once for each packet.
  //
  // So every time this routine runs, we need to read ALL the packets
  // that are now available;
  //
  // We do this by calling IAudioCaptureClient.GetNextPacketSize
  // over and over again until it indicates there are no more packets remaining.

  // The original code: while SUCCEEDED(m_AudioCaptureClient.GetNextPacketSize(FramesAvailable)) and (FramesAvailable > 0) do

  // We check the device state first and then call IAudioCaptureClient.GetNextPacketSize.
  // This way we handle the internal async calls that could interfere first.
  while (m_DeviceState = Capturing) do
    begin

      if not Assigned(m_AudioCaptureClient) then
        Break;
      // We don't use the buffersize from the captureclient, because for some reason this will cause random hick-ups.
      // Using the same buffersize as the audioclient will solve the hick-up issues.
      hr := m_AudioCaptureClient.GetNextPacketSize(FramesAvailable);
      //hr := m_AudioClient.GetBufferSize(FramesAvailable);
      if FAILED(hr) then
        begin
          StopCaptureAsync();
          Break;
        end;

      if (FramesAvailable = 0) then
        Break;

      cbBytesToCapture := (FramesAvailable * m_CaptureFormat.nBlockAlign);

      // WAV files have a 4GB ($FFFFFFFF) size limit on 32 bit platforms.
      // So, most likely we have hit that limit when we overflow here.
      // Time to stop the capture.
      if ((m_cbDataSize + cbBytesToCapture) < m_cbDataSize) then
        begin
          StopCaptureAsync();
          Break;
        end;

      // Create a new buffer.
      pBufferData := AllocMem(FramesAvailable);

      // Get sample buffer.
      hr := m_AudioCaptureClient.GetBuffer(pData,
                                           FramesAvailable,
                                           @dwCaptureFlags,
                                           nil,
                                           nil);
      if FAILED(hr) then
        begin
           ErrMsg(Format('m_AudioCaptureClient.GetBuffer failed. LastError = %d',[GetLastError()]), hr);
           Break;
        end;

      // Write silence.
      // When a sound is detected, the app will act and process data.
      if (dwCaptureFlags = AUDCLNT_BUFFERFLAGS_SILENT) or (dwCaptureFlags = AUDCLNT_BUFFERFLAGS_DATA_DISCONTINUITY) then
        begin
          m_AudioCaptureClient.ReleaseBuffer(FramesAvailable);
          pData := nil;
          Break;
        end;

      try
        // Copy data to a new buffer.
        Move(pData^,
             pBufferData^,
             cbBytesToCapture);

        // Release buffer.
        m_AudioCaptureClient.ReleaseBuffer(FramesAvailable);

        // Write to file.
        if (m_DeviceState = Capturing) then
          begin

            dwBytesWritten := 0;
            br := WriteFile(m_hPipe,
                            pBufferData^,
                            cbBytesToCapture,
                            dwBytesWritten,
                            nil);
            if (br = False) then
              begin
                ErrMsg(Format('%s LastError = %d', [SysErrorMessage(GetLastError), GetLastError()]), E_FAIL);
                FreeMem(pBufferData);
                Break;
              end;
          end;

      finally
        FreeMem(pBufferData);
      end;

      // Increase the size of our 'data' chunk. m_cbDataSize needs to be accurate.
      Inc(m_cbDataSize,
          cbBytesToCapture);

      Inc(NumBytesWritten,
          dwBytesWritten);

      // Store to public.
      pvBytesWritten := dwBytesWritten;

      pData := nil;
    end;

leave:
  pData := nil;
  Result := hr;
end;