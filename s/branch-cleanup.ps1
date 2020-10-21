# prune deleted remoted branches
git fetch -p

# get all branches and their corresponding remote status
# deleted remotes will be marked [gone]
git branch -v |
  #find ones marked [gone], capture branchName
  select-string -Pattern '^  (?<branchName>\S+)\s+\w+ \[gone\]' | 
  foreach-object{ 
     #delete the captured branchname.
     git branch -D $_.Matches[0].Groups['branchName']
}
