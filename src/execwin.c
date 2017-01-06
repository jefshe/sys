#ifdef _WIN32
#include <Rinternals.h>
#include <windows.h>


/* Check for interrupt without long jumping */
void check_interrupt_fn(void *dummy) {
  R_CheckUserInterrupt();
}

int pending_interrupt() {
  return !(R_ToplevelExec(check_interrupt_fn, NULL));
}

BOOL CALLBACK closeWindows(HWND hWnd, LPARAM lpid) {
  DWORD pid = (DWORD)lpid;
  DWORD win;
  GetWindowThreadProcessId(hWnd, &win);
  if(pid == win)
    CloseWindow(hWnd);
  return TRUE;
}

SEXP C_run_with_pid(SEXP command, SEXP args, SEXP wait){
  SECURITY_ATTRIBUTES sa;
  PROCESS_INFORMATION pi = {0};
  STARTUPINFO si = {0};
  si.cb = sizeof(STARTUPINFO);
  const char * cmd = CHAR(STRING_ELT(command, 0));
  char argv[MAX_PATH];
  argv[0] = '\0';
  for(int i = 0; i < Rf_length(args); i++){
    strcat(argv, CHAR(STRING_ELT(args, i)));
    strcat(argv, " ");
  }
  sa.nLength = sizeof(sa);
  sa.lpSecurityDescriptor = NULL;
  sa.bInheritHandle = TRUE;
  if(!CreateProcess(cmd, argv, &sa, &sa, TRUE, CREATE_NO_WINDOW, NULL, NULL, &si, &pi))
    Rf_errorcall(R_NilValue, "CreateProcess failed for %s", cmd);

  //CloseHandle(pi.hThread);
  DWORD pid = GetProcessId(pi.hProcess);
  HANDLE proc = pi.hProcess;
  HANDLE thread = pi.hThread;
  if(asLogical(wait)){
    while (WAIT_TIMEOUT == WaitForSingleObject(proc, 500)) {
      if(pending_interrupt()){
        EnumWindows(closeWindows, pid);
        if(!TerminateThread(thread, 99))
          Rf_errorcall(R_NilValue, "TerminateThread failed %d", GetLastError());
        if(!TerminateProcess(proc, 99))
          Rf_errorcall(R_NilValue, "TerminateProcess failed: %d", GetLastError());
      }
    }
    DWORD exit_code;
    GetExitCodeProcess(proc, &exit_code);
    return ScalarInteger(exit_code);
  }
  CloseHandle(thread);
  CloseHandle(proc);
  return ScalarInteger(pid);

}
#endif
