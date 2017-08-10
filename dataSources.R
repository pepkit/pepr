# parse data sources like python can do with format()
# Currently non-functional; just some notes.


library(stringr)

str_locate_all(cfg$ewing$data_sources$bsf_samples, "\\{.*?\\}")
res = str_extract_all(cfg$ewing$data_sources$bsf_samples, "\\{.*?\\}")
result = cfg$ewing$data_sources$bsf_samples
for (r in res[[1]]) { 
	res_stripped = str_replace_all(r, "[{}]", "")
	message(r, res_stripped)
	result = gsub(r, res_stripped, result)
}
result 
str_match_all(result, "\\{.*?\\}")
str_match_all(result, "\\{.*?\\}")[[1]]

str_replace_all(cfg$ewing$data_sources$bsf_samples, "\\{.*?\\}", "blah")
str_replace_all(cfg$ewing$data_sources$bsf_samples, "\\{.*?\\}", "\1")

psaProj[1,]

library(stringi)

result = "{sample_name}/{age_class}"
vars = stri_match_all(result, regex = "\\{.*?\\}", vectorize_all = FALSE)[[1]][,1]
strip = stri_sub(vars, 2, -2)

myreplacer = function(x, y, z) {
	for (i in seq_len(y)) {
		a = y[i]
		b = z[i]
		x = stringi::stri_replace_all_fixed(x, a, get(b), vectorize_all = TRUE)
	}
}

vals = list(sample_name="samp1", age_class="20-30")

myreplacer(result, vars, vals)



psaProj[, myreplacer(result, vars, get(strip))]

apply(psaProj, 1, stringi::stri_replace_all_fixed(result, vars, get(strip), vectorize_all = FALSE))

lapply(strip, function(x) { psaProj[[x]]})
psaProj[["flowcell"]]
psaProj







strformat = function(str, vals) {
	vars = stringi::stri_match_all(str, regex = "\\{.*?\\}", vectorize_all = FALSE)[[1]][,1]
	varsStrip = stringi::stri_sub(vars, 2, -2)
	x = str
	for (i in seq_along(names(vals))) {
		varName = names(vals)[i]
		varCode = paste0("{", varName, "}")
		x = stringi::stri_replace_all_fixed(x, varCode, vals[[varName]], vectorize_all = TRUE)
	}
	return(x)
}


str = "Sammy the {animal} {verb} a {noun}."
vals = list(animal="shark", verb="ate", noun="fish")

strformat(str, vals)

vars=dict(animal="shark", verb="ate", noun="fish")
string="Sammy the {animal} {verb} a {noun}."
print(string.format(**vars)) 

form = function(s,L){
 s = gsub("\\}", "%>", gsub("\\{","<%=",s))
 brew(text=s, envir=as.environment(L))
}

form(str, vals)


str2 = gsub("\\}", ")", gsub("\\{", ".(", str))
bquote("Sammy the .(animal) {verb} a {noun}.", as.environment(vals)) 