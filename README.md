# yat

YAT is a simple console tracker for pet projects  based on files and directories.

## Instalation
```
git pull https://github.com/zewalldev/yat.git
cd yat
stack install
```
To install stack аollow the  [instructions](https://docs.haskellstack.org/en/stable/install_and_upgrade/)

## Available commands

Initialize yat:
```
yat init
```
Creation, starting and finish tasks:
```
yat request <key>
yat start <key>
yat finish <key>
```
Starting and finishing releases
```
yat start release <version>
yat finish release <version>
```
List tasks:
```
yat list requested
yat list inprogress
yat list done
```
*where <key> must match the pattern `<letter><letter|digit|->*` e. g. ma-task1 and `<version>` can be an any string.*

## Hook support
You can configure hooks for **request**, **start** and **finish** commands.
In order to do this, you need to create executable files in the directory `.yat/conf/hooks`. 
The name of the executable file must match the pattern `<pre|post>_<command_name>_<todo|release>`.
For example, when executing a `yat request my-task`, the following hooks will be called sequentially:
```
pre_request_todo
post_request_todo
```
and the task key **my-task**  will be passed to these scripts as the first argument `$1`.
An example for supporting git flow can be found in [git flow hooks](.yat/conf/hooks/)