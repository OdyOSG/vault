# vault
 Accessor package for downloading OHDSI goodies


## How to use

### Set up PAT

First need to set up git credentials via a Personal Access Toekn (PAT). This is best done using the `gitcreds` package. If you have a `GITHUB_PAT` set in your .Renviron, you should reconfigure this to work with the `gitcreds` package as this is the most secure way to handle PATs. To do this, first create a PAT in the developer settings. Once a new PAT is created run:

```r
gitcreds::gitcreds_set()
```

In the console, paste in your new PAT and set the credentials. You must update this set if your PAT expires. Once this is set you can delete the `GITHUB_PAT` set in your .Renviron. For more thorough instructions follow the documentation of `gitcreds_set`.

### Accessing vaults

Once you have set up your PAT using `gitcreds`, you can now access a vault by providing a correct string specifying `<org>/<repo>`. With this string you can check if you have vault access or list the contents of the vault.

```r
vault::checkVaultAccess("OdyOSG/picardScripts")
vault::vaultContents("OdyOSG/picardScripts")
```

### Downloading Contents

Todo...
