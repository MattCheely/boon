# Boon

Sometimes your application might need to let users configure some basic boolean logic
rules which then have to be stored in a database somewhere. Maybe you have logic 
that needs to be shared across multiple platforms, or edited by non-technical
contributors.

Boon, the **bo**lean expressi**on** language, is a useful tool for these situations. 
It looks like this:

```boon
# Check if your pulled pork has the correct BBQ sauce
isVinegarBased AND NOT (sugaryKetchup OR mustard)
```

Boon supports the following operators (in descending precedence):

 - NOT
 - XOR
 - AND
 - OR

Parentheses can be used to override precedence. Quotation marks can be used for data 
identifiers that need spaces or other special characters. Comments start with a `#`. 
There is a [full specification](https://docs.google.com/document/d/1UzsnnKjjW7T_u-OPb5dcPmc9My4YS_jHoyButolNVa4/edit?usp=sharing) 
if you need more detail, but here are some examples to get you started:

```boon
# Great Sci-Fi Authors
"Octavia Butler" AND "N.K. Jemisin"
```

&nbsp;    
  
```boon
# We need exactly three Hugos for best novel
NOT "Lois McMaster Bujold" # She won too many!
AND 
("N.K. Jemisin"          # The Broken Earth Trilogy - Three in a row!
  OR 
  ("Ann Leckie"          # Ancillary Justice
    AND "Arkady Martine" # A Memory Called Empire
    AND "Martha Wells"   # Network Effect
  )
)
```

## Notes

If you need similar behavior in JS, try [boon-js](https://github.com/jakec-github/boon-js), which I believe is the
origin of the Boon spec. 
