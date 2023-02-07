# How to test your plugin's changes on real files

You've made some changes to this repo on your local machine, and now you probably want to test it against your own project repo.

The `dprint` script in this folder is a convenient wrapper that makes it easy to build and call your work-in-progress dprint and plugins from anywhere on your system. It will always rebuild dprint and the typescript plugin if necessary.

To use it:
1. In the `dprint` script, set `DPRINT_PLUGIN_TYPESCRIPT_SOURCE_PATH` to wherever this repo is cloned. You can find that by running

```sh
git rev-parse --show-toplevel
```

2. If you have your own `dprint.json` config, then inside the `dprint.json` in this `development` folder, change the `extends` field to the absolute path of your `dprint.json`. For example, if my project repo filetree looks like the following, then I would specify `extends: "/Users/david/work/my-project/dprint.json"`.

```
/
└--_ Users
   └--_ david
      └--_ work
         |--  dprint-plugin-typescript
         └--_ my-project
            |--  dprint.json
            └--_ src
               | ...
```

3. Run the `dprint` script inside this folder.