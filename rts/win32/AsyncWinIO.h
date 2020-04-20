/* AsyncIO.h
 *
 * Integrating Win32 asynchronous IOCP with the GHC RTS.
 *
 * (c) Tamar Christina, 2018
 *
 * NOTE: This is the WinIO manager, only used for --io-manager=native.
 *       For the MIO manager see AsyncIO.h.
 */

#pragma once

#include "Rts.h"
#include <stdbool.h>
#include <windows.h>

extern bool startupAsyncWinIO(void);
extern void shutdownAsyncWinIO(bool wait_threads);
extern void awaitAsyncRequests(bool wait);
extern void registerNewIOCPHandle (HANDLE port);
extern void registerAlertableWait (HANDLE port, DWORD mssec, uint64_t num_req);

extern OVERLAPPED_ENTRY* getOverlappedEntries (uint32_t *num);
extern void servicedIOEntries (uint64_t remaining);
extern void completeSynchronousRequest (void);
<<<<<<< HEAD

=======
extern bool queueIOThread(void);
>>>>>>> f1207f6124... winio: nontreaded: Create io processing threads in main thread.

