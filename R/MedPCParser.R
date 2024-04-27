################################################################################
PullStringFromTextSearch = function(allText, text){

# Pulls string following specified text
#
# Args:
#   allText: Character string containing the MedPC file
#   text: A character string where matches are searched for
#
# Returns:
#   A character string following the matched text

  identifiedString = grep(text, allText, value = TRUE)
  identifiedString_elements=strsplit(identifiedString," ")
  stringValue = tail(identifiedString_elements[[1]], n=1)
  return(stringValue)
}


################################################################################
PullStringFromIndex = function(allText, index){

# Pulls indices - used to pull main variables from B_index
#
# Args:
#   allText: Character string containing the MedPC file
#   index: A number specifying the index where the array or value of interest
#          begins in the MedPC file
#
# Returns:
#   A character string following the index start

  indexString = allText[index]
  indexString_elements = strsplit(indexString," ")
  stringValue = tail(indexString_elements[[1]], n=1)
  return(stringValue)
}


################################################################################
DecimalStringToInteger = function(string){
# Converts string into integer - used with main variables from B_index
#
# Args:
#   string: Character to be converted into an integer
#
# Returns:
#   An integer

  string_noDecimals = strsplit(string,".",fixed=T)[[1]][1]
  integer = strtoi(string_noDecimals)
  return(integer)
}


################################################################################
TimestampPull = function(allText,HMSindex){
# Pulls timestamps in H:M:S format
#
# Args:
#   allText: Character string containing the MedPC file
#   HMSindex: A number specifying the index to pull from the H, M, and S arrays
#
# Returns:
#   A single character combining the values from the H, M, and S arrays to
#   read a complete time stamp (H:M:S)

  tempH_index = 1+grep("H:", allText[-1], value = FALSE)
  tempM_index = 1+grep("M:", allText[-1], value = FALSE)
  tempS_index = 1+grep("S:", allText[-1], value = FALSE)

  Hstring = allText[tempH_index+HMSindex+1]
  Hstring_elements = strsplit(Hstring, " ")
  H_decimal = tail(Hstring_elements[[1]]," ", n=1)
  H = strsplit(H_decimal,".",fixed=T)[[1]][1]

  Mstring = allText[tempM_index+HMSindex+1]
  Mstring_elements = strsplit(Mstring, " ")
  M_decimal = tail(Mstring_elements[[1]]," ", n=1)
  M_nodecimal = strsplit(M_decimal,".",fixed=T)[[1]][1]

  if (nchar(M_nodecimal)==1)
  {
    M = paste0("0",M_nodecimal)
  }
  else
  {
    M = M_nodecimal
  }

  Sstring = allText[tempS_index+HMSindex+1]
  Sstring_elements = strsplit(Sstring, " ")
  S_decimal = tail(Sstring_elements[[1]]," ", n=1)
  S_nodecimal = strsplit(S_decimal,".",fixed=T)[[1]][1]
  if (nchar(S_nodecimal)==1)
  {
    S = paste0("0",S_nodecimal)
  }
  else
  {
    S = S_nodecimal
  }

  HMS=paste0(H,":",M,":",S)
  return(HMS)
}
