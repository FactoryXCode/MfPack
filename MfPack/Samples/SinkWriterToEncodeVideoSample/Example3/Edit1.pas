
        // Get sample buffer
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
            Continue;
          end;

        // Write to file.
        if (m_DeviceState <> Stopping) or (m_DeviceState <> Stopped) or (m_DeviceState <> Error) then
          begin
            dwBytesWritten := 0;
            br := WriteFile(m_hPipe,
                            pwData^,
                            cbBytesToCapture,
                            dwBytesWritten,
                            nil);

            if (br = False) then
              begin
                ErrMsg(Format('%s LastError = %d',[SysErrorMessage(GetLastError), GetLastError()]), E_FAIL);
                Break;
              end;
          end;

        // Release buffer.
        m_AudioCaptureClient.ReleaseBuffer(FramesAvailable);

