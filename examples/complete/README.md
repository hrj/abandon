### File structure

#### `accounts.conf`
This is the configuration file that is given to `abandon`. To run this example, you can invoke this command:

```
abandon_cli.sh -c accounts.conf
```

`abandon` will parse all the input files specified in the configuration file and generate report files in this
directory.

The format of the config file is a superset of JSON. It has some syntax extensions to make it human-friendly.

#### Input files
Inside `abandon.conf` you will notice this declaration:
```
inputs += accounts.ledger
```

This specifies that the file `accounts.ledger` is an input file.

> *Note:* You can specify more than one input file to `abandon`. There are two ways to do this. One
> is to declare them in the config file, the other is to use the `include` directive inside the `.ledger`
> file.
> This example uses the `include` directive.
