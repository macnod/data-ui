# Demo Walkthroughs

Guided tours of the live applications introduced in [Live Demos](../README.md#live-demos). Each e-demo walkthrough logs in as an ordinary user, shows you something the model is hiding from that user, then logs in again as a user holding the role that reveals it. Same data, same compiled app — the only variable is who you are. That is RBAC working end to end, and all of it was decided by the model, not by per-user code.

## Conventions

- **Guest access.** Every app — e-demo or demo — accepts the login `guest` with no password. Guests are read-only.
- **Demo users.** The e-demo apps (To Do List, Books & Authors) publish demo users with write access: `demos`, `alice`, and `bob`. All three share one public password: `TryDataUI2026!`. The roles they hold are what the stories below turn on.
- **Nightly reset.** The e-demo apps reset to a known state every morning at 04:10 (US Pacific). Anything you change is gone by then; that is the point of a shared demo.
- **On-request access.** The demo apps (currently Model Bank) are persistent. Beyond `guest`, credentials are available on request.

## To Do List

**Link:** https://todo-stg.demo.data-ui.com/ **Login:** `demos` / `TryDataUI2026!`

Log in as `demos` and look around. The list is missing something: any item tagged both `mvp` and `frontend` that is not yet `Done` is invisible to you. Here's one way to see what's missing:

(Keep in mind that the system resets every day at 04:10 (US Pacific), and that the row references in the steps are for a reset system. If other users come along during the day and make changes, the references will be off.)

1. Ensure the list in the `todos` tab is sorted by `Done`, in ascending order (this is the default, from the model).
2. Find and click an `mvp` chip. (There's one on the 6th row, "More example models".)
3. Now find and click a `frontend` chip. (You'll find one toward the bottom, among the first rows that are marked `Done`, "Login button blues".)
4. Note how all the remaining items are marked `Done`!

Now click the Admin link near the top right to see the RBAC types. The `roles` list includes a role `frontend`, and if you check the `users` list you will see that `alice` and `bob` hold it. They can see those hidden items. Nothing was coded per user; the model scopes the view by role, and the compiled app enforces it.

Log out and back in as `alice` (same password as `demos`) and the missing items appear. Just follow steps 1-4 above. This time, for Step 2 you'll find an `mvp` chip on the 4th row, "Cursor in first field on Edit forms". For Step 3, you'll find a `frontend` chip in the first row, "Cursor in first field on Edit forms". And, in Step 4, you'll find 9 rows where `Done` isn't checked. One of those rows appears to have no role. That's the "Break up App.tsx" row, which is assigned exclusively to `alice`. Thus, `bob` can see everything `alice` can see (they both hold the `frontend` role), except for the "Break up App.tsx" row, which is visible to `alice` only.

## Books & Authors

**Link:** https://books-stg.demo.data-ui.com/ **Login:** `demos` / `TryDataUI2026!`

A similar story in a different shape. As `demos` you can see most of the library, but books tagged with both genres `Nonfiction` and `Self-Help` are withheld. Those require the `new-books` role, which only `alice` and `bob` hold.

Log in as `alice` (same password) and the self-help shelf appears.

## Model Bank

**Link:** https://modelbank.demo.data-ui.com/ **Login:** `guest` — no password, read-only.

A gallery of models with ownership, images, and ratings. Anything beyond `guest` — write access, including the Generate and Deploy buttons — is available on request: [contact Donnie](https://sinistercode.com/public/donnie/contact).

---

More demos are on the way; this page will grow with them.
