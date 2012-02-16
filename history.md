# History

## 0.5.4
* Introduction of the new text system which makes it much easier to
  create AttributedStrings. See the clarity.text namespace.
* Added support for immutable table models and viewing table models as
  seqs. See clarity.table namespace.
* Ability to implement interfaces/protocols and override methods of
  based classes via (make) and (do-component). Like so:
  
    (make [:button :has-value]
      (:impl (.getText [] "foo")
             (value [] 10)))
  
* Minor combo box hack that allows a combo list which is wider than
  the box itself. clarity.widgets/set-combo-list-width
