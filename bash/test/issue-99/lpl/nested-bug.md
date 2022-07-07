# Testfile

A shared block

```ts {#shared/setting}
// Sync me everywhere
```

Applied to another block

```ts {#shared/config}
// So another
```

And used in 2 files

A:

```ts {file=a.ts}
<<shared/config>>
```

B:

```ts {file=b.ts}
<<shared/config>>
```

C:

```ts {file=c.ts}
<<shared/setting>>
```
