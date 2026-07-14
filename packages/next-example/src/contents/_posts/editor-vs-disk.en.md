---
title: "When an Agent Reads a File, Is It Reading the Buffer or the Disk?"
date: 2026-07-13
draft: false
categories:
  - Systems
tags:
  - editor
  - agent
  - zed
  - consistency
---

<div class="admonition" data-admonition-type="note">
An LLM was used to research sources, read Zed's source code, edit the prose, and translate this article.
</div>

## A Mental Model: Two Copies

When you open an editor, you think you are editing a file. More precisely, you are editing a **buffer**. The path on disk is something else.

For a human, the classic data flow looks roughly like this:

```
 disk  ----(watch)---->  buffer  ----(UI)---->  human
  ^                         |
  |--------- save ----------|
```

A few default assumptions have made this arrangement good enough for decades:

- Most of the time, the same person edits the buffer and clicks Save.
- A conflict can open a dialog; the human can stop and think.
- If the watcher is tens of milliseconds late, the human barely notices.
- To the user, "the current file contents" are whatever appears in the buffer on screen.

Once an agent enters the picture, the diagram becomes this:

```
 human ----edit----> buffer <----agent read/write----
                       ^  \
                       |   \---- save ----\
                       |                   v
 external tools ---- disk <---- shell / fmt / git ----
                       ^
                       |
                    watcher (eventually)
```

The agent now plays three roles at once: an observer that reads the buffer, a collaborator that writes it, and an "external process" that can run shell commands against disk. Worse, it often **creates an external change itself and immediately reads again**. The system races with itself.

I will use the [Zed editor](https://zed.dev/) as the concrete example.
Zed connects external coding agents such as Codex and Claude Code through [ACP (Agent Client Protocol)](https://agentclientprotocol.com/).

In a local integration, the editor acts as the client and launches the agent subprocess; they communicate over `JSON-RPC` on `stdio`. Requests flow in both directions: the editor sends prompts to the agent, while the agent can ask the editor to read or write a file, create a terminal, or request permission from the user.

The two requests that matter here are `fs/read_text_file` and `fs/write_text_file`, sent by the agent to the editor. Their names sound like path APIs, but Zed first calls `project.open_buffer`. A read returns text from a buffer snapshot. A write computes a diff against a snapshot, applies it to the buffer through anchors, and finally calls `save_buffer`. In other words, Zed's choice is closer to:

> Read and write the **buffer object in the session**, then call `save_buffer` at the end of a write.

It is not a versioned POSIX operation on the path every time. Either choice can be coherent. The awkward part is that the agent also runs `echo`, `cargo fmt`, or code generators, and those touch disk.

After a shell command changes disk, Zed still needs a file watcher to update the buffer. If the buffer contains unsaved edits, that update cannot simply overwrite them. This is where two apparently equivalent "files" begin to diverge.

Zed's contract for the two ACP file requests is clear: they read and write the buffer. But the agent can also run a shell in a terminal and bypass those requests to access disk directly. Once a single tool loop interleaves the two routes, the synchronization between buffer and disk becomes observable. All four scenarios below happen at that boundary.

## Four Concrete Scenarios

All four diagrams may look like variations of "the agent read an old file," but they reach different branches in the code. First, here is what Zed does in each case:

1. The shell has changed disk, but Zed has not processed the watcher event yet. This is a timing window; once the event is processed, a clean buffer reloads.
2. The watcher event has been processed, but neither mtime nor size changed. Zed sees the same disk state and does not reload.
3. The buffer has unsaved edits. Zed deliberately keeps the buffer and records an external conflict; an ACP read still returns only the buffer text to the agent.
4. An ACP write edits Zed's CRDT buffer and can merge with a human's unsaved changes. A shell modification to disk does not use that collaboration path.

These cases are independent. In each sequence diagram below, vertical lines are participants, labels on arrows are observable actions, and `???` marks the point where behavior becomes surprising.

### 1. The External Write Finished, but the Watcher Has Not Settled

An agent tool loop often does this: modify disk through the shell, then immediately issue an ACP read, without waiting for the event loop to drain. If the read only snapshots an open buffer and `file_updated` has not happened yet, the second read still returns the old text even though disk already contains `v2`.

```
  agent              buffer              disk               watcher
    |                   |                  |                    |
    |-- read_text ----->|                  |                    |
    |   open / snapshot |                  |                    |
    |<-- "v1" ----------|                  |                    |
    |                   |                  |                    |
    |-- shell: echo v2 > foo ------------->|                    |
    |                   |                  |-- inotify (soon) ->|
    |                   |                  |                    |
    |-- read_text ----->|                  |                    |
    |   (no wait)       |                  |                    |
    |   snapshot again  |  still "v1"      |  content = "v2"    |  event maybe
    |<-- ??? "v1" ------|                  |                    |  not processed
    |                   |                  |                    |
    |                   |<-- file_updated -|<- process later ---|
    |                   |   (too late for that read)            |
```

The event may still be in flight, or it may already be readable from the fd while the application has not reloaded the buffer. The observable result is the same for the agent: a stale read.

In Zed, whether this read sees the new disk contents depends on whether the watcher has propagated the change to the buffer in time. That path is normally fast in an ordinary project. If another model call happens before the next read, the buffer has usually reloaded. The window is easier to hit when the shell command and read are adjacent.

### 2. Neither mtime nor Size Changed

After Zed receives a file event, it does not immediately compare old and new contents. `Buffer::file_updated` compares two `DiskState` values; for a file that exists on disk, that state contains only mtime and size. Reloading therefore depends on this predicate:

```
changed = (mtime' != mtime) || (size' != size)
```

An in-place rewrite that preserves both length and mtime leaves `old_state == new_state`. The **watcher has settled and `file_updated` has run, but Zed still emits no `ReloadNeeded`**. This is a safety problem, not a liveness problem.

```
  agent              buffer              disk               file_updated
    |                   |                  |                    |
    |-- read ---------->|                  |                    |
    |<-- "fn foo ( ) {" |  size=12         |  mtime=T0          |
    |                   |  saved meta=T0   |                    |
    |                   |                  |                    |
    |            external rewrite, same size, mtime stays T0    |
    |                   |                  |-- "fn foo() {  " ->|
    |                   |                  |   size=12, mtime=T0|
    |                   |                  |                    |
    |              watcher fully settles                        |
    |                   |                  |                    |
    |                   |<-- file_updated -|--------------------|
    |                   |   old (T0,12)    |                    |
    |                   |   new (T0,12)    |                    |
    |                   |   equal? yes ----|--> no ReloadNeeded |
    |                   |  content still   |  content already   |
    |                   |  "fn foo ( ) {"  |  "fn foo() {  "    |
    |                   |                  |                    |
    |-- read ---------->|                  |                    |
    |<-- ??? stale -----|                  |                    |
```

With nanosecond mtimes on modern ext4, an everyday formatter is unlikely to land on the exact same mtime. The predicate still trusts metadata. Once metadata lies, the event loop can finish while the system remains convinced that nothing changed.

### 3. A Dirty Buffer Does Not Reload Automatically

Keeping unsaved edits is an intentional Zed behavior. Suppose a human inserts `HUMAN` into the buffer without saving. The agent then runs `echo 'DISK_V2' > file` in a terminal. The watcher tells Zed that disk changed, but because the buffer is dirty, Zed does not replace it with `DISK_V2`; it enters a conflict state instead.

If the agent now sends `fs/read_text_file`, it still receives the unsaved `HUMAN\nv1`. The agent itself initiated the shell write, yet it cannot tell from the read response that disk now contains `DISK_V2`, because the response carries text but no conflict state.

```
  human              buffer              disk               agent
    |                   |                  |                  |
    |-- edit ---------->|                  |                  |
    |   insert HUMAN    |  dirty=true      |  "v1"            |
    |                   |  text=HUMAN+v1   |                  |
    |                   |                  |                  |
    |                   |<-- read_text --------------------|
    |                   |--- "HUMAN\nv1" ------------------>|
    |                   |                  |                  |
    |         agent: echo DISK_V2 > file   |                  |
    |                   |                  |<-- DISK_V2 ------|
    |                   |                  |                  |
    |              watcher settles         |                  |
    |                   |                  |                  |
    |                   |  was_dirty? yes  |                  |
    |                   |  => no reload    |                  |
    |                   |  has_conflict=1  |  disk=DISK_V2    |
    |                   |  text still      |                  |
    |                   |  HUMAN+v1        |                  |
    |                   |                  |                  |
    |                   |<-- read_text --------------------|
    |                   |--- ??? "HUMAN\nv1" -------------->|
    |                   |   (no conflict in API)              |
```

An error would at least stop the next step. Silently returning an old string lets the agent continue reasoning from the wrong state.

### 4. A Write Meets Two Kinds of Concurrent Change

Between an agent's read and write, a human may edit the file, or a shell may change it. The first change happens in the buffer; the second happens on disk. Zed handles them differently.

**4a. The human and agent both edit the buffer**

```
  human              buffer                 agent
    |                   |                      |
    |                   |  base = one\ntwo\nthree\n
    |                   |                      |
    |                   |<-- read -------------|
    |                   |--- base ------------>|
    |                   |                      |
    |-- edit head ----->|                      |
    |   ZERO\n + base   |  dirty               |
    |                   |                      |
    |                   |<-- write (append four)
    |                   |   diff vs snapshot   |
    |                   |   anchor apply       |
    |                   |  => ZERO\none\n..\nfour\n
    |                   |                      |
    |                   |--- Ok / merge ------>|
```

Zed has no problem in this scenario. Its buffer is a CRDT, and an ACP write does not overwrite the file directly. It computes a text diff against the snapshot from the previous read, then uses anchors to apply the edit to the current buffer. The human's `ZERO` and the agent's `four` touch different places, so both survive.

**4b. A shell rewrites disk after the agent's read**

Consider a one-line file. The agent reads `mode = "dev"` and plans to change it to `mode = "test"`. Before the write, another shell has already changed disk to `mode = "prod"`:

```
  agent                 buffer                 disk                  shell
    |                       |                    |                      |
    |-- read ------------->|                    |                      |
    |<-- mode = "dev" -----|                    |  mode = "dev"        |
    |                       |                    |                      |
    |                       |                    |<-- mode = "prod" ----|
    |                       |   reload or not    |                      |
    |                       |                    |                      |
    |-- write mode="test" >|                    |                      |
    |   based on "dev"      |-- edit + save --->|                      |
    |                       |                    |  mode = "test"       |
    |<-- Ok ----------------|                    |  "prod" is gone      |
```

Whether or not the watcher reloads in time, this write may overwrite `mode = "prod"`. The agent made its decision from the old value `dev`, but the ACP request does not say which version it read. Zed can only apply the edit, save, and return success.

If the interface requires "disk has not changed since the last read," the read must return a revision and the write must send it back for comparison. A mismatch rejects the write and forces the agent to read again. That is the compare-and-swap discussed here. It is one possible interface constraint, and it is not something the CRDT merge in 4a can solve automatically.

---

Of the four scenarios, the first looks most like a classic timing race. A natural reaction is to ask: is Linux inotify often too slow?

## A Small Experiment: How Quickly Does the Watcher Fire on Local ext4?

On the machine used for this article, both `/tmp` and `/home` are ext4. A raw `inotify` test (`IN_MODIFY` | `IN_CLOSE_WRITE`) measures **kernel delivery**, not Zed's entire worktree pipeline. But if this layer is already extremely fast, "the OS notification was slow" is a poor default explanation for stale reads.

| Measurement | Result (order of magnitude) |
|-------------|-----------------------------|
| From `close` returning to inotify becoming readable | p50 **~10 µs** |
| Nonblocking `select` ready immediately after `close` | **~100%** |
| Event already on the fd when `bash -c 'echo > f'` returns | **Almost always** (the shell itself takes ~1–2 ms) |
| Path read immediately sees new contents after a write | **Yes** |

At the same time, an application path that "returns only the open buffer and does not process watch events in this turn" stays **consistently stale** relative to disk, regardless of whether inotify has already delivered the event. Once events are drained and the buffer reloads, the stale rate falls to zero.

Compare that with a real agent timeline. One LLM tool round trip usually takes hundreds of milliseconds to several seconds, far longer than inotify. Across multiple turns, the gap between "the shell changed a file" and "the model thought for a while and read it again" is normally enough for the watcher to settle. The sensitive cases are **a shell command immediately followed by a read in the same tool sequence**, or **a read path that never aligns with disk before returning**.

So, for a local disk and the kernel side of the pipeline:

> **The watcher is unlikely to be "too slow to fire."**  
> If the agent still reads old text, it is more likely reading a buffer, or the application has not turned an already-delivered event into a reload.

## Bonus: When There Are Too Many Files, "the Watcher Never Fires"

The experiment above asks whether an event is readable from the **inotify fd**. An editor has a much longer pipeline:

```
inotify -> worktree process_events -> UpdatedEntries -> file_updated -> ReloadNeeded -> open/read disk
```

If any layer drops the event, both the UI and the agent experience it as "the watcher did not fire."

Once, I had two Zed windows open in the same process. One contained an almost empty minimal directory. The other contained a monorepo with a huge `node_modules` at its root, plus this Yarn workspace symlink:

```text
node_modules/next-example -> ../packages/next-example
```

The comparison looked like this:

- An open buffer in the minimal window, the monorepo root, or a small package reloaded quickly after an external disk change.
- An open buffer under **a package linked into `node_modules` by the workspace** did not. inotify still produced the filename, but **`file_updated` never ran**, the file was never reopened from disk, and the buffer remained on the old contents.

Following the pipeline, the good path was roughly: inotify event -> worktree entry update -> `file_updated` -> reopen the disk file. The bad path stopped in the middle: the kernel delivered the event, but the buffer never reloaded. Zed's `worktree::process_events` contains a filter that closely matches this behavior:

```rust
// The parent must be EntryKind::Dir; drop events under an UnloadedDir or a missing entry.
if !parent_dir_is_loaded {
    log::debug!("filtering event {relative_path:?} within unloaded directory");
    // Drop it instead of updating the entry.
}
```

These logs are nearly invisible under default settings, so the experience is simply "the watcher broke." A more precise description is:

> **The kernel watch fired. When the file tree is huge or its scan/ignore state is messy, the worktree may drop the event at the application layer.**

The trigger tends to combine several conditions: a huge dependency tree that was not ignored, duplicate paths through workspace symlinks, and a long-running session where a subtree became an `UnloadedDir` while an inotify watch remained attached. A minimal project almost never sees it. A monorepo whose ignore rules miss the root `node_modules` is much easier to push into this state.

That is the bonus, and the warning: **an event on the fd** and **a business-level reload** are different facts.

## Conclusion

Returning to Zed's choice, `fs/read_text_file` and `fs/write_text_file` operate on the buffer. That preserves a human's unsaved edits and lets non-overlapping human and agent changes merge through the CRDT. Once the same agent opens a terminal and writes directly to disk, the system gains another route; disk changes need the watcher to reach the buffer.

The local ext4 experiment confines the first scenario to a narrow window. By the time an `echo` process returns, the inotify event is almost always readable. In an ordinary project, another model call usually gives Zed enough time to reload. If the read is still stale, the next question is whether the worktree processed or dropped the event. That small timing window becomes the leading suspect only when the shell and read are adjacent.

The four scenarios leave four open questions:

1. Should `fs/read_text_file` promise a buffer snapshot or the current contents on disk? If it chooses the buffer, how can the caller explicitly request alignment with, or a direct read from, disk?
2. When a file event arrives but mtime and size are identical, should the editor trust metadata or compare contents or a hash? Who pays for the latter?
3. Should a conflict appear in the read response? The interface could return only text, attach a revision or status, or fail outright; each choice gives the agent different information.
4. When a write discovers that its base changed, should it try to merge, allow last-write-wins, or reject the operation with compare-and-swap? Should buffer edits and disk bypasses follow the same rule?

Push those questions back one step: which copy is the editor's SSOT? The boundary was already explicit in the [Unix Second Edition `ed` manual from 1972](https://www.tuhs.org/Archive/Distributions/Research/1972_stuff/unix_2nd_edition_manual.pdf). `ed` edited a copy of a file; changes happened in the buffer and reached the original file only when the user ran `w`. More surprisingly, the buffer itself lived in a temporary file. Buffer versus disk was never simply a physical RAM-versus-disk distinction. It separated "the working copy being edited" from "the named, persistent file used by other programs." Inside an editing session, the buffer is current. To a compiler or shell, the named file is the only state it can see.

Later full-screen editors added cursors, undo, selections, and multiple windows to the working copy without changing that basic boundary. ProseMirror sits closer to the buffer-only end and leaves persistence to its host. A path transformation such as `sed -i` sits closer to the disk-only end. A modern code editor needs both: the buffer carries unfinished edits, while disk serves compilers, formatters, Git, and the shell.

[Claude Code](https://docs.anthropic.com/en/docs/claude-code/getting-started) and [Codex CLI](https://help.openai.com/en/articles/11096431) can work without going through an editor at all. They enter a working directory, read paths, write directly to disk, and invoke formatters, compilers, and tests through the shell. An agent does not need a cursor, a selection, or the unsaved text on screen. If it owns the worktree for the duration of the task, disk is a handoff surface understood by every tool. The conflicts in this article therefore do not arise in an unattended agentic loop.

Human-agent co-work, however, is not going away soon. I expect the problems in this article to keep tormenting **long-tail** users for another year or two. When the watcher stops working and the agent edits the wrong file, perhaps this article will be worth another look.
