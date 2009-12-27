" Vimball Archiver by Charles E. Campbell, Jr., Ph.D.
UseVimball
finish
after/ftplugin/actionscript_snippets.vim
9
if !exists('loaded_snippet') || &cp
    finish
endif

let st = g:snip_start_tag
let et = g:snip_end_tag
let cd = g:snip_elem_delim

exec "Snippet dm duplicateMovieClip(".st."target".et.", ".st."newName".et.", ".st."depth".et.");"
after/ftplugin/aspvbs_snippets.vim
17
if !exists('loaded_snippet') || &cp
    finish
endif

let st = g:snip_start_tag
let et = g:snip_end_tag
let cd = g:snip_elem_delim

exec "Snippet rr Response.Redirect(".st."to".et.")".st.et.""
exec "Snippet app Application(\"".st.et."\")".st.et.""
exec "Snippet forin For ".st."var".et." in ".st."array".et."<CR>".st.et."<CR>Next<CR>".st.et.""
exec "Snippet ifelse If ".st."condition".et." Then<CR>".st.et."<CR>Else<CR>".st.et."<CR>End if<CR>".st.et.""
exec "Snippet rw Response.Write ".st.et.""
exec "Snippet sess Session(\"".st.et."\")".st.et.""
exec "Snippet rf Request.Form(\"".st.et."\")".st.et.""
exec "Snippet rq Request.QueryString(\"".st.et."\")".st.et.""
exec "Snippet while While ".st."NOT".et." ".st."condition".et."<CR>".st.et."<CR>Wend<CR>".st.et.""
after/ftplugin/c_snippets.vim
58
if !exists('loaded_snippet') || &cp
    finish
endif

function! Count(haystack, needle)
    let counter = 0
    let index = match(a:haystack, a:needle)
    while index > -1
        let counter = counter + 1
        let index = match(a:haystack, a:needle, index+1)
    endwhile
    return counter
endfunction

function! CArgList(count)
    " This returns a list of empty tags to be used as 
    " argument list placeholders for the call to printf
    let st = g:snip_start_tag
    let et = g:snip_end_tag
    if a:count == 0
        return ""
    else
        return repeat(', '.st.et, a:count)
    endif
endfunction
	
function! CMacroName(filename)
    let name = a:filename
    let name = substitute(name, '\.','_','g')
    let name = substitute(name, '\(.\)','\u\1','g')
    return name
endfunction

let st = g:snip_start_tag
let et = g:snip_end_tag
let cd = g:snip_elem_delim

exec "Snippet do do<CR>{<CR>".st.et."<CR>} while (".st.et.");".st.et
exec "Snippet readfile std::vector<uint8_t> v;<CR>if(FILE* fp = fopen(\"".st."filename".et."\", \"r\"))<CR>{<CR>uint8_t buf[1024];<CR>while(size_t len = fread(buf, 1, sizeof(buf), fp))<CR>v.insert(v.end(), buf, buf + len);<CR>fclose(fp);<CR>}<CR>".st.et
exec "Snippet beginend ".st."v".et.".begin(), ".st."v".et.".end()".st.et
exec "Snippet once #ifndef ``CMacroName(expand('%'))``_<CR><CR>#define ``CMacroName(expand('%'))``_<CR><CR>".st.et."<CR><CR>#endif /* ``CMacroName(expand('%'))``_ */<CR>"
"exec "Snippet once #ifndef _".st."file:substitute(expand('%'),'\\(.\\)','\\u\\1','g')".et."_<CR><CR>#define _".st."file".et."_<CR><CR>".st.et."<CR><CR>#endif /* _".st."file".et."_ */<CR>".st.et
exec "Snippet class class ".st."name".et."<CR>{<CR>public:<CR>".st."name".et." (".st."arguments".et.");<CR>virtual ~".st."name".et."();<CR><CR>private:<CR>".st.et."<CR>};<CR>".st.et
" TODO This is a good one but I can't quite work out the syntax yet
exec "Snippet printf printf(\"".st."\"%s\"".et."\\n\"".st."\"%s\":CArgList(Count(@z, '%[^%]'))".et.");<CR>".st.et
exec "Snippet vector std::vector<".st."char".et."> v".st.et.";"
exec "Snippet struct struct ".st."name".et."<CR>{<CR>".st.et."<CR>};<CR>".st.et
exec "Snippet template template <typename ".st."_InputIter".et."><CR>".st.et
" TODO this one as well. Wish I knew more C
" Snippet namespace namespace ${1:${TM_FILENAME/(.*?)\\..*/\\L$1/}}\n{\n\t$0\n};<CR>.st.et
exec "Snippet namespace namespace ".st.":substitute(expand('%'),'.','\\l&', 'g')".et."<CR>{<CR>".st.et."<CR>};<CR>".st.et
exec "Snippet map std::map<".st."key".et.", ".st."value".et."> map".st.et.";<CR>".st.et
exec "Snippet mark #if 0<CR><CR>".st.et."<CR><CR>#endif<CR><CR>".st.et
exec "Snippet if if(".st.et.")<CR>{<CR>".st.et."<CR>}<CR>".st.et
exec "Snippet main int main (int argc, char const* argv[])<CR>{<CR>".st.et."<CR>return 0;<CR>}<CR>".st.et
exec "Snippet Inc #include <".st.et."><CR>".st.et
exec "Snippet inc #include \"".st.et.".h\"".st.et
exec "Snippet for for( ".st.et." ".st."i".et." = ".st.et."; ".st."i".et." < ".st."count".et."; ".st."i".et." += ".st.et.")<CR>{<CR>".st.et."<CR>}<CR>".st.et
after/ftplugin/css_snippets.vim
30
if !exists('loaded_snippet') || &cp
    finish
endif

let st = g:snip_start_tag
let et = g:snip_end_tag
let cd = g:snip_elem_delim

exec "Snippet visibility ".st.et.";".st.et
exec "Snippet list list-style-image: url(".st.et.");".st.et
exec "Snippet text text-shadow: rgb(".st.et.", ".st.et.", ".st.et.", ".st.et." ".st.et." ".st.et.";".st.et
exec "Snippet overflow overflow: ".st.et.";".st.et
exec "Snippet white white-space: ".st.et.";".st.et
exec "Snippet clear cursor: url(".st.et.");".st.et
exec "Snippet margin padding-top: ".st.et.";".st.et
exec "Snippet background background #".st.et." url(".st.et.") ".st.et." ".st.et." top left/top center/top right/center left/center center/center right/bottom left/bottom center/bottom right/x% y%/x-pos y-pos')".et.";".st.et
exec "Snippet word word-spaceing: ".st.et.";".st.et
exec "Snippet z z-index: ".st.et.";".st.et
exec "Snippet vertical vertical-align: ".st.et.";".st.et
exec "Snippet marker marker-offset: ".st.et.";".st.et
exec "Snippet cursor cursor: ".st.et.";".st.et
exec "Snippet border border-right: ".st.et."px ".st.et." #".st.et.";".st.et
exec "Snippet display display: block;".st.et
exec "Snippet padding padding: ".st.et." ".st.et.";".st.et
exec "Snippet letter letter-spacing: ".st.et."em;".st.et
exec "Snippet color color: rgb(".st.et.", ".st.et.", ".st.et.");".st.et
exec "Snippet font font-weight: ".st.et.";".st.et
exec "Snippet position position: ".st.et.";".st.et
exec "Snippet direction direction: ".st.et.";".st.et
exec "Snippet float float: ".st.et.";".st.et
after/ftplugin/django_model_snippets.vim
61
if !exists('loaded_snippet') || &cp
    finish
endif

function! Count(haystack, needle)
    let counter = 0
    let index = match(a:haystack, a:needle)
    while index > -1
        let counter = counter + 1
        let index = match(a:haystack, a:needle, index+1)
    endwhile
    return counter
endfunction

function! DjangoArgList(count)
    " This needs to be Python specific as print expects a
    " tuple and an empty tuple looks like this (,) so we'll need to make a
    " special case for it
    let st = g:snip_start_tag
    let et = g:snip_end_tag
    if a:count == 0
        return "()"
    else
        return '('.repeat(st.et.', ', a:count).')'
    endif
endfunction

let st = g:snip_start_tag
let et = g:snip_end_tag
let cd = g:snip_elem_delim

exec "Snippet mmodel class ".st.et."(models.Model):<CR>\"\"\"".st.et."\"\"\"<CR>".st.et." = ".st.et."<CR><CR>class Admin:<CR>pass<CR><CR>def __str__(self):<CR>return \"".st."s".et."\" % ".st."s:DjangoArgList(Count(@z, '%[^%]'))".et."<CR>".st.et
exec "Snippet mauto models.AutoField(".st.et.")".st.et
exec "Snippet mbool models.BooleanField()".st.et
exec "Snippet mchar models.CharField(maxlength=".st."50".et.st.et.")".st.et
exec "Snippet mcsi models.CommaSeparatedIntegerField(maxlength=".st."50".et.st.et.")".st.et
exec "Snippet mdate models.DateField(".st.et.")".st.et
exec "Snippet mdatet models.DateTimeField(".st.et.")".st.et
exec "Snippet memail models.EmailField(".st.et.")".st.et
exec "Snippet mfile models.FileField(upload_to=\"".st.et."\"".st.et.")".st.et
exec "Snippet mfilep models.FilePathField(path=\"".st.et."\"".st.et.")".st.et
exec "Snippet mfloat models.FloatField(max_digits=".st.et.", decimal_places=".st.et.")".st.et
exec "Snippet mimage models.ImageField(".st.et.")".st.et
exec "Snippet mint models.IntegerField(".st.et.")".st.et
exec "Snippet mipadd models.IPAddressField(".st.et.")".st.et
exec "Snippet mnull models.NullBooleanField()".st.et
exec "Snippet mphone models.PhoneNumberField(".st.et.")".st.et
exec "Snippet mpint models.PositiveIntegerField(".st.et.")".st.et
exec "Snippet mspint models.PositiveSmallIntegerField(".st.et.")".st.et
exec "Snippet mslug models.SlugField(".st.et.")".st.et
exec "Snippet msint models.SmallIntegerField(".st.et.")".st.et
exec "Snippet mtext models.TextField(".st.et.")".st.et
exec "Snippet mtime models.TimeField(".st.et.")".st.et
exec "Snippet murl models.URLField(verify_exists=".st."True".et.st.et.")".st.et
exec "Snippet muss models.USStateField(".st.et.")".st.et
exec "Snippet mxml models.XMLField(schema_path=\"".st.et."\"".st.et.")".st.et
exec "Snippet mfor models.ForeignKey(".st.et.")".st.et
exec "Snippet mm2o models.ForeignKey(".st.et.")".st.et
exec "Snippet mm2m models.ManyToManyField(".st.et.")".st.et
exec "Snippet mo2o models.OneToOneField(".st.et.")".st.et
exec "Snippet mman models.Manager()".st.et
after/ftplugin/django_template_snippets.vim
32
if !exists('loaded_snippet') || &cp
    finish
endif

let st = g:snip_start_tag
let et = g:snip_end_tag
let cd = g:snip_elem_delim

exec "Snippet {{ {% templatetag openvariable %}".st.et
exec "Snippet }} {% templatetag closevariable %}".st.et
exec "Snippet {% {% templatetag openblock %}".st.et
exec "Snippet %} {% templatetag closeblock %}".st.et
exec "Snippet now {% now \"".st.et."\" %}".st.et
exec "Snippet firstof {% firstof ".st.et." %}".st.et
exec "Snippet ifequal {% ifequal ".st.et." ".st.et." %}<CR>".st.et."<CR>{% endifequal %}<CR>".st.et
exec "Snippet ifchanged {% ifchanged %}".st.et."{% endifchanged %}".st.et
exec "Snippet regroup {% regroup ".st.et." by ".st.et." as ".st.et." %}".st.et
exec "Snippet extends {% extends \"".st.et."\" %}<CR>".st.et
exec "Snippet filter {% filter ".st.et." %}<CR>".st.et."<CR>{% endfilter %}".st.et
exec "Snippet block {% block ".st.et." %}<CR>".st.et."<CR>{% endblock %}<CR>".st.et
exec "Snippet cycle {% cycle ".st.et." as ".st.et." %}".st.et
exec "Snippet if {% if ".st.et." %}<CR>".st.et."<CR>{% endif %}<CR>".st.et
exec "Snippet debug {% debug %}<CR>".st.et
exec "Snippet ifnotequal {% ifnotequal ".st.et." ".st.et." %}<CR>".st.et."<CR>{% endifnotequal %}<CR>".st.et
exec "Snippet include {% include ".st.et." %}<CR>".st.et
exec "Snippet comment {% comment %}<CR>".st.et."<CR>{% endcomment %}<CR>".st.et
exec "Snippet for {% for ".st.et." in ".st.et." %}<CR>".st.et."<CR>{% endfor %}<CR>".st.et
exec "Snippet ssi {% ssi ".st.et." ".st.et." %}".st.et
exec "Snippet widthratio {% widthratio ".st.et." ".st.et." ".st.et." %}".st.et
exec "Snippet load {% load ".st.et." %}<CR>".st.et
" Field snippet contributed by Alex Pounds
exec "Snippet field <p><label for=\"id_".st."fieldname".et."\">".st."fieldlabel".et.":</label> {{ form.".st."fieldname".et." }}<CR>{% if form.".st."fieldname".et.".errors %}*** {{ form.".st."fieldname".et.".errors|join:\", \" }} {% endif %}</p>".st.et
after/ftplugin/f-script_snippets.vim
14
if !exists('loaded_snippet') || &cp
    finish
endif

let st = g:snip_start_tag
let et = g:snip_end_tag
let cd = g:snip_elem_delim

exec "Snippet tbd to:".st.et." by:".st.et." do:[ ".st.et." |<CR>".st.et."<CR>].".st.et
exec "Snippet it ifTrue:[<CR>".st.et."<CR>].".st.et
exec "Snippet ift ifFalse:[<CR>".st.et."<CR>] ifTrue:[<CR>".st.et."<CR>].".st.et
exec "Snippet itf ifTrue:[<CR>".st.et."<CR>] ifFalse:[<CR>".st.et."<CR>].".st.et
exec "Snippet td to:".st.et." do:[".st.et." ".st.et." |<CR>".st.et."<CR>].".st.et
exec "Snippet if ifFalse:[<CR>".st.et."<CR>].".st.et
after/ftplugin/haskell_snippets.vim
9
if !exists('loaded_snippet') || &cp
    finish
endif

let st = g:snip_start_tag
let et = g:snip_end_tag
let cd = g:snip_elem_delim

exec "Snippet mod module: ".st.et." where<CR><Tab>".st.et
after/ftplugin/html_snippets.vim
57
if !exists('loaded_snippet') || &cp
    finish
endif

let st = g:snip_start_tag
let et = g:snip_end_tag
let cd = g:snip_elem_delim

function! SelectDoctype()
    let st = g:snip_start_tag
    let et = g:snip_end_tag
    let cd = g:snip_elem_delim
    let dt = inputlist(['Select doctype:',
                \ '1. HTML 4.01',
                \ '2. HTML 4.01 Transitional',
                \ '3. HTML 4.01 Frameset',
                \ '4. XHTML 1.0 Frameset',
                \ '5. XHTML Strict',
                \ '6. XHTML Transitional',
                \ '7. XHTML Frameset'])
    let dts = {1: "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\"\n\"http://www.w3.org/TR/html4/strict.dtd\">\n".st.et,
             \ 2: "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\"\n\"http://www.w3.org/TR/html4/loose.dtd\">\n".st.et,
             \ 3: "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Frameset//EN\"\n\"http://www.w3.org/TR/html4/frameset.dtd\">\n".st.et,
             \ 4: "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Frameset//EN\"\n\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-frameset.dtd\">\n".st.et,
             \ 5: "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML Strict//EN\"\n\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">\n".st.et,
             \ 6: "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML Transitional//EN\"\n\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">\n".st.et,
             \ 7: "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML Frameset//EN\"\n\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-frameset.dtd\">\n".st.et}
    
    return dts[dt]
endfunction

exec "Snippet doct ``SelectDoctype()``"
exec "Snippet doctype <!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Frameset//EN\"<CR><TAB>\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-frameset.dtd\"><CR>".st.et
exec "Snippet doc4s <!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\"<CR>\"http://www.w3.org/TR/html4/strict.dtd\"><CR>".st.et
exec "Snippet doc4t <!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\"<CR>\"http://www.w3.org/TR/html4/loose.dtd\"><CR>".st.et
exec "Snippet doc4f <!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Frameset//EN\"<CR>\"http://www.w3.org/TR/html4/frameset.dtd\"><CR>".st.et
exec "Snippet docxs <!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML Strict//EN\"<CR>\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\"><CR>".st.et
exec "Snippet docxt <!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML Transitional//EN\"<CR>\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\"><CR>".st.et
exec "Snippet docxf <!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML Frameset//EN\"<CR>\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-frameset.dtd\"><CR>".st.et
exec "Snippet head <head><CR><meta http-equiv=\"Content-type\" content=\"text/html; charset=utf-8\" /><CR><title>".st.et."</title><CR>".st.et."<CR></head><CR>".st.et
exec "Snippet script <script type=\"text/javascript\" language=\"javascript\" charset=\"utf-8\"><CR>// <![CDATA[<CR><TAB>".st.et."<CR>// ]]><CR></script><CR>".st.et
exec "Snippet title <title>".st.et."</title>"
exec "Snippet body <body id=\"".st.et."\" ".st.et."><CR>".st.et."<CR></body><CR>".st.et
exec "Snippet scriptsrc <script src=\"".st.et."\" type=\"text/javascript\" language=\"".st.et."\" charset=\"".st.et."\"></script><CR>".st.et
exec "Snippet textarea <textarea name=\"".st.et."\" rows=\"".st.et."\" cols=\"".st.et."\">".st.et."</textarea><CR>".st.et
exec "Snippet meta <meta name=\"".st.et."\" content=\"".st.et."\" /><CR>".st.et
exec "Snippet movie <object width=\"".st.et."\" height=\"".st.et."\"<CR>classid=\"clsid:02BF25D5-8C17-4B23-BC80-D3488ABDDC6B\"<CR>codebase=\"http://www.apple.com/qtactivex/qtplugin.cab\"><CR><param name=\"src\"<CR>value=\"".st.et."\" /><CR><param name=\"controller\" value=\"".st.et."\" /><CR><param name=\"autoplay\" value=\"".st.et."\" /><CR><embed src=\"".st.et."\"<CR>width=\"".st.et."\" height=\"".st.et."\"<CR>controller=\"".st.et."\" autoplay=\"".st.et."\"<CR>scale=\"tofit\" cache=\"true\"<CR>pluginspage=\"http://www.apple.com/quicktime/download/\"<CR>/><CR></object><CR>".st.et
exec "Snippet div <div ".st.et."><CR>".st.et."<CR></div><CR>".st.et
exec "Snippet mailto <a href=\"mailto:".st.et."?subject=".st.et."\">".st.et."</a>".st.et
exec "Snippet table <table border=\"".st.et."\"".st.et." cellpadding=\"".st.et."\"><CR><tr><th>".st.et."</th></tr><CR><tr><td>".st.et."</td></tr><CR></table>"
exec "Snippet link <link rel=\"".st.et."\" href=\"".st.et."\" type=\"text/css\" media=\"".st.et."\" title=\"".st.et."\" charset=\"".st.et."\" />"
exec "Snippet form <form action=\"".st.et."\" method=\"".st.et."\"><CR>".st.et."<CR><CR><p><input type=\"submit\" value=\"Continue &rarr;\" /></p><CR></form><CR>".st.et
exec "Snippet ref <a href=\"".st.et."\">".st.et."</a>".st.et
exec "Snippet h1 <h1 id=\"".st.et."\">".st.et."</h1>".st.et
exec "Snippet input <input type=\"".st.et."\" name=\"".st.et."\" value=\"".st.et."\" ".st.et."/>".st.et
exec "Snippet style <style type=\"text/css\" media=\"screen\"><CR>/* <![CDATA[ */<CR>".st.et."<CR>/* ]]> */<CR></style><CR>".st.et
exec "Snippet base <base href=\"".st.et."\"".st.et." />".st.et
after/ftplugin/java_snippets.vim
52
if !exists('loaded_snippet') || &cp
    finish
endif

function! UpFirst()
    return substitute(@z,'.','\u&','')
endfunction

function! JavaTestFileName(type)
    let filepath = expand('%:p')
    let filepath = substitute(filepath, '/','.','g')
    let filepath = substitute(filepath, '^.\(:\\\)\?','','')
    let filepath = substitute(filepath, '\','.','g')
    let filepath = substitute(filepath, ' ','','g')
    let filepath = substitute(filepath, '.*test.','','')
    if a:type == 1
        let filepath = substitute(filepath, '.[A-Za-z]*.java','','g')
    elseif a:type == 2
        let filepath = substitute(filepath, 'Tests.java','','')
    elseif a:type == 3
        let filepath = substitute(filepath, '.*\.\([A-Za-z]*\).java','\1','g')
    elseif a:type == 4
        let filepath = substitute(filepath, 'Tests.java','','')
        let filepath = substitute(filepath, '.*\.\([A-Za-z]*\).java','\1','g')
    elseif a:type == 5
        let filepath = substitute(filepath, 'Tests.java','','')
        let filepath = substitute(filepath, '.*\.\([A-Za-z]*\).java','\1','g')
        let filepath = substitute(filepath, '.','\l&','')
    endif

    return filepath
endfunction

let st = g:snip_start_tag
let et = g:snip_end_tag
let cd = g:snip_elem_delim

exec "Snippet method // {{{ ".st."method".et."<CR>/**<CR> * ".st.et."<CR> */<CR>public ".st."return".et." ".st."method".et."() {<CR>".st.et."}<CR>// }}}<CR>".st.et
exec "Snippet jps private static final ".st."string".et." ".st.et." = \"".st.et."\";<CR>".st.et
exec "Snippet jtc try {<CR>".st.et."<CR>} catch (".st.et." e) {<CR>".st.et."<CR>} finally {<CR>".st.et."<CR>}<CR>".st.et
exec "Snippet jlog /** Logger for this class and subclasses. */<CR><CR>protected final Log log = LogFactory.getLog(getClass());<CR>".st.et
exec "Snippet jpv private ".st."string".et." ".st.et.";<CR><CR>".st.et
exec "Snippet bean // {{{ set".st."fieldName:UpFirst()".et."<CR>/**<CR> * Setter for ".st."fieldName".et.".<CR> * @param new".st."fieldName:UpFirst()".et." new value for ".st."fieldName".et."<CR> */<CR>public void set".st."fieldName:UpFirst()".et."(".st."String".et." new".st."fieldName:UpFirst()".et.") {<CR>".st."fieldName".et." = new".st."fieldName:UpFirst()".et.";<CR>}<CR>// }}}<CR><CR>// {{{ get".st."fieldName:UpFirst()".et."<CR>/**<CR> * Getter for ".st."fieldName".et.".<CR> * @return ".st."fieldName".et." */<CR>public ".st."String".et." get".st."fieldName:UpFirst()".et."() {<CR>return ".st."fieldName".et.";<CR>}<CR>// }}}<CR>".st.et
exec "Snippet jwh while (".st.et.") { // ".st.et."<CR><CR>".st.et."<CR><CR>}<CR>".st.et
exec "Snippet sout System.out.println(\"".st.et."\");".st.et
exec "Snippet jtest package ".st."j:JavaTestFileName(1)".et."<CR><CR>import junit.framework.TestCase;<CR>import ".st."j:JavaTestFileName(2)".et.";<CR><CR>/**<CR> * ".st."j:JavaTestFileName(3)".et."<CR> *<CR> * @author ".st.et."<CR> * @since ".st.et."<CR> */<CR>public class ".st."j:JavaTestFileName(3)".et." extends TestCase {<CR><CR>private ".st."j:JavaTestFileName(4)".et." ".st."j:JavaTestFileName(5)".et.";<CR><CR>public ".st."j:JavaTestFileName(4)".et." get".st."j:JavaTestFileName(4)".et."() { return this.".st."j:JavaTestFileName(5)".et."; }<CR>public void set".st."j:JavaTestFileName(4)".et."(".st."j:JavaTestFileName(4)".et." ".st."j:JavaTestFileName(5)".et.") { this.".st."j:JavaTestFileName(5)".et." = ".st."j:JavaTestFileName(5)".et."; }<CR><CR>public void test".st.et."() {<CR>".st.et."<CR>}<CR>}<CR>".st.et
exec "Snippet jif if (".st.et.") { // ".st.et."<CR>".st.et."<CR>}<CR>".st.et
exec "Snippet jelse if (".st.et.") { // ".st.et."<CR><CR>".st.et."<CR><CR>} else { // ".st.et."<CR>".st.et."<CR>}<CR>".st.et
exec "Snippet jpm /**<CR> * ".st.et."<CR> *<CR> * @param ".st.et." ".st.et."<CR> * ".st.et." ".st.et."<CR> */<CR>private ".st."void".et." ".st.et."(".st."String".et." ".st.et.") {<CR><CR>".st.et."<CR><CR>}<CR>".st.et
exec "Snippet main public main static void main(String[] ars) {<CR>".st."\"System.exit(0)\"".et.";<CR>}<CR>".st.et
exec "Snippet jpum /**<CR> * ".st.et."<CR> *<CR> * @param ".st.et." ".st.et."<CR> *".st.et." ".st.et."<CR> */<CR>public ".st."void".et." ".st.et."(".st."String".et." ".st.et.") {<CR><CR>".st.et."<CR><CR>}<CR>".st.et
exec "Snippet jcout <c:out value=\"${".st.et."}\" />".st.et
after/ftplugin/javascript_snippets.vim
10
if !exists('loaded_snippet') || &cp
    finish
endif

let st = g:snip_start_tag
let et = g:snip_end_tag
let cd = g:snip_elem_delim

exec "Snippet proto ".st."className".et.".prototype.".st."methodName".et." = function(".st.et.")<CR>{<CR>".st.et."<CR>};<CR>".st.et
exec "Snippet fun function ".st."functionName".et." (".st.et.")<CR>{<CR><Tab>".st.et."<CR><BS>}<CR>".st.et
after/ftplugin/latex_snippets.vim
13
if !exists('loaded_snippet') || &cp
    finish
endif

let st = g:snip_start_tag
let et = g:snip_end_tag
let cd = g:snip_elem_delim

exec "Snippet sub \\subsection{".st."name".et."}\\label{sub:".st."name:substitute(@z,'.','\\l&','g')".et."}<CR>".st.et
exec "Snippet $$ \\[<CR>".st.et."<CR>\\]<CR>".st.et
exec "Snippet ssub \\subsubsection{".st."name".et."}\\label{ssub:".st."name:substitute(@z,'.','\\l&','g')".et."}<CR>".st.et
exec "Snippet itd \\item[".st."desc".et."] ".st.et
exec "Snippet sec \\section{".st."name".et."}\\label{sec:".st."name:substitute(@z,'.','\\l&','g')".et."}<CR>".st.et
after/ftplugin/logo_snippets.vim
9
if !exists('loaded_snippet') || &cp
    finish
endif

let st = g:snip_start_tag
let et = g:snip_end_tag
let cd = g:snip_elem_delim

exec "Snippet to to ".st."name".et." ".st."argument".et."<CR>".st.et."<CR>end<CR>".st.et
after/ftplugin/markdown_snippets.vim
10
if !exists('loaded_snippet') || &cp
    finish
endif

let st = g:snip_start_tag
let et = g:snip_end_tag
let cd = g:snip_elem_delim

exec "Snippet img ![".st."altText".et."](".st."SRC".et.")".st.et
exec "Snippet link [".st."desc".et."](".st."HREF".et.")".st.et
after/ftplugin/movable_type_snippets.vim
14
if !exists('loaded_snippet') || &cp
    finish
endif

let st = g:snip_start_tag
let et = g:snip_end_tag
let cd = g:snip_elem_delim

exec "Snippet cat <$MTCategoryDescription$>".st.et
exec "Snippet blog <$MTBlogName$>".st.et
exec "Snippet archive <$MTArchiveFile$>".st.et
exec "Snippet cal <MTCalendarIfEntries><CR><Tab>".st.et."<CR></MTCalendarIfEntries><CR>".st.et
exec "Snippet entry <$MTEntryMore$>".st.et
exec "Snippet entries <MTEntriesHeader><CR><Tab>".st.et."<CR></MTEntriesHeader><CR>".st.et
after/ftplugin/objc_snippets.vim
53
if !exists('loaded_snippet') || &cp
    finish
endif

function! UpFirst()
    return substitute(@z,'.','\u&','')
endfunction

function! Count(haystack, needle)
    let counter = 0
    let index = match(a:haystack, a:needle)
    while index > -1
        let counter = counter + 1
        let index = match(a:haystack, a:needle, index+1)
    endwhile
    return counter
endfunction

function! ObjCArgList(count)
    let st = g:snip_start_tag
    let et = g:snip_end_tag

    if a:count == 0
        return st.et
    else
        return st.et.repeat(', '.st.et, a:count)
    endif
endfunction


let st = g:snip_start_tag
let et = g:snip_end_tag
let cd = g:snip_elem_delim

exec "Snippet cat @interface ".st."NSObject".et." (".st."Category".et.")<CR><CR>@end<CR><CR><CR>@implementation ".st."NSObject".et." (".st."Category".et.")<CR><CR>".st.et."<CR><CR>@end<CR>".st.et
exec "Snippet delacc - (id)delegate;<CR><CR>- (void)setDelegate:(id)delegate;<CR>".st.et
exec "Snippet ibo IBOutlet ".st."NSSomeClass".et." *".st."someClass".et.";<CR>".st.et
exec "Snippet dict NSMutableDictionary *".st."dict".et." = [NSMutableDictionary dictionary];<CR>".st.et
exec "Snippet Imp #import <".st.et.".h><CR>".st.et
exec "Snippet objc @interface ".st."class".et." : ".st."NSObject".et."<CR>{<CR>}<CR>@end<CR><CR>@implementation ".st."class".et."<CR>- (id)init<CR>{<CR>self = [super init]; <CR>if (self != nil)<CR>{<CR>".st.et."<CR>}<CR>return self;<CR>}<CR>@end<CR>".st.et
exec "Snippet imp #import \"".st.et.".h\"<CR>".st.et
exec "Snippet bez NSBezierPath *".st."path".et." = [NSBezierPath bezierPath];<CR>".st.et
exec "Snippet acc - (".st."\"unsigned int\"".et.")".st."thing".et."<CR>{<CR>return ".st."fThing".et.";<CR>}<CR><CR>- (void)set".st."thing:UpFirst()".et.":(".st."\"unsigned int\"".et.")new".st."thing:UpFirst()".et."<CR>{<CR>".st."fThing".et." = new".st."thing:UpFirst()".et.";<CR>}<CR>".st.et
exec "Snippet format [NSString stringWithFormat:@\"".st.et."\", ".st.et."]".st.et
exec "Snippet focus [self lockFocus];<CR><CR>".st.et."<CR><CR>[self unlockFocus];<CR>".st.et
exec "Snippet setprefs [[NSUserDefaults standardUserDefaults] setObject:".st."object".et." forKey:".st."key".et."];<CR>".st.et
exec "Snippet log NSLog(@\"%s".st."s".et."\", ".st."s:ObjCArgList(Count(@z, '%[^%]'))".et.");".st.et
exec "Snippet gsave [NSGraphicsContext saveGraphicsState];<CR>".st.et."<CR>[NSGraphicsContext restoreGraphicsState];<CR>".st.et
exec "Snippet forarray for(unsigned int index = 0; index < [".st."array".et." count]; index += 1)<CR>{<CR>".st."id".et."object = [".st."array".et." objectAtIndex:index];<CR>".st.et."<CR>}".st.et
exec "Snippet classi @interface ".st."ClassName".et." : ".st."NSObject".et."<CR><CR>{".st.et."<CR><CR>}<CR><CR>".st.et."<CR><CR>@end<CR>".st.et
exec "Snippet array NSMutableArray *".st."array".et." = [NSMutableArray array];".st.et
exec "Snippet getprefs [[NSUserDefaults standardUserDefaults] objectForKey:<key>];".st.et
exec "Snippet cati @interface ".st."NSObject".et." (".st."Category".et.")<CR><CR>".st.et."<CR><CR>@end<CR>".st.et
after/ftplugin/ocaml_snippets.vim
26
if !exists('loaded_snippet') || &cp
    finish
endif

let st = g:snip_start_tag
let et = g:snip_end_tag
let cd = g:snip_elem_delim

exec "Snippet Queue Queue.fold ".st.et." ".st."base".et." ".st."q".et."<CR>".st.et
exec "Snippet Nativeint Nativeint.abs ".st."ni".et.st.et
exec "Snippet Printexc Printexc.print ".st."fn".et." ".st."x".et.st.et
exec "Snippet Sys Sys.Signal_ignore".st.et
exec "Snippet Hashtbl Hashtbl.iter ".st.et." ".st."h".et.st.et
exec "Snippet Array Array.map ".st.et." ".st."arr".et.st.et
exec "Snippet Printf Printf.fprintf ".st."buf".et." \"".st."format".et."\" ".st."args".et.st.et
exec "Snippet Stream Stream.iter ".st.et." ".st."stream".et.st.et
exec "Snippet Buffer Buffer.add_channel ".st."buf".et." ".st."ic".et." ".st."len".et.st.et
exec "Snippet Int32 Int32.abs ".st."i32".et.st.et
exec "Snippet List List.rev_map ".st.et." ".st."lst".et.st.et
exec "Snippet Scanf Scanf.bscaf ".st."sbuf".et." \"".st."format".et."\" ".st."f".et.st.et
exec "Snippet Int64 Int64.abs ".st."i64".et.st.et
exec "Snippet Map Map.Make ".st.et
exec "Snippet String String.iter ".st.et." ".st."str".et.st.et
exec "Snippet Genlex Genlex.make_lexer ".st."\"tok_lst\"".et." ".st."\"char_stream\"".et.st.et
exec "Snippet for for ".st."i}".et." = ".st.et." to ".st.et." do<CR>".st.et."<CR>done<CR>".st.et
exec "Snippet Stack Stack.iter ".st.et." ".st."stk".et.st.et
after/ftplugin/perl_snippets.vim
23
if !exists('loaded_snippet') || &cp
    finish
endif

let st = g:snip_start_tag
let et = g:snip_end_tag
let cd = g:snip_elem_delim

exec "Snippet sub sub ".st."FunctionName".et." {<CR>".st.et."<CR>}<CR>".st.et
exec "Snippet class package ".st."ClassName".et.";<CR><CR>".st.et.st."ParentClass".et.st.et.";<CR><CR>sub new {<CR>my \$class = shift;<CR>\$class = ref \$class if ref \$class;<CR>my $self = bless {}, \$class;<CR>\$self;<CR>}<CR><CR>1;<CR>".st.et
exec "Snippet xfore ".st."expression".et." foreach @".st."array".et.";".st.et
exec "Snippet xwhile ".st."expression".et." while ".st."condition".et.";".st.et
exec "Snippet xunless ".st."expression".et." unless ".st."condition".et.";".st.et
exec "Snippet slurp my $".st."var".et.";<CR><CR>{ local $/ = undef; local *FILE; open FILE, \"<".st."file".et.">\"; $".st."var".et." = <FILE>; close FILE }".st.et
exec "Snippet if if (".st.et.") {<CR>".st.et."<CR>}<CR>".st.et
exec "Snippet unless unless (".st.et.") {<CR>".st.et."<CR>}<CR>".st.et
exec "Snippet ifee if (".st.et.") {<CR>".st.et."<CR><BS>} elsif (".st.et.") {<CR>".st.et."<CR><BS>} else {<CR>".st.et."<CR>}<CR><CR>".st.et
exec "Snippet ife if (".st.et.") {<CR>".st.et."<CR>} else {<CR>".st.et."<CR>}<CR>".st.et
exec "Snippet for for (my \$".st."var".et." = 0; \$".st."var".et." < ".st."expression".et."; \$".st."var".et."++) {<CR>".st.et."<CR>}<CR>".st.et
exec "Snippet fore foreach my \$".st."var".et." (@".st."array".et.") {<CR>".st.et."<CR>}<CR>".st.et
exec "Snippet eval eval {<CR>".st.et."<CR>};<CR>if ($@) {<CR>".st.et."<CR>}<CR>".st.et
exec "Snippet while while (".st.et.") {<CR>".st.et."<CR>}<CR>".st.et
exec "Snippet xif ".st."expression".et." if ".st."condition".et.";".st.et
after/ftplugin/php_snippets.vim
30
if !exists('loaded_snippet') || &cp
    finish
endif

let st = g:snip_start_tag
let et = g:snip_end_tag
let cd = g:snip_elem_delim

exec "Snippet elseif elseif ( ".st."condition".et." )<CR>{<CR><Tab>".st.et."<CR>}<CR>".st.et
exec "Snippet do do<CR>{<CR>".st.et."<CR><CR>} while ( ".st.et." );<CR>".st.et
exec "Snippet reql require_once( '".st."file".et."' );<CR>".st.et
exec "Snippet if? $".st."retVal".et." = ( ".st."condition".et." ) ? ".st."a".et." : ".st."b".et." ;<CR>".st.et
exec "Snippet phpp <?php<CR><CR>".st.et."<CR><CR>?>"
exec "Snippet switch switch ( ".st."variable".et." )<CR>{<CR>case '".st."value".et."':<CR>".st.et."<CR>break;<CR><CR>".st.et."<CR><CR>default:<CR>".st.et."<CR>break;<CR>}<CR>".st.et
exec "Snippet class #doc<CR>#classname:".st."ClassName".et."<CR>#scope:".st."PUBLIC".et."<CR>#<CR>#/doc<CR><CR>class ".st."ClassName".et." ".st."extendsAnotherClass".et."<CR>{<CR>#internal variables<CR><CR>#Constructor<CR>function __construct ( ".st."argument".et.")<CR>{<CR>".st.et."<CR>}<CR>###<CR><CR>}<CR>###".st.et
exec "Snippet incll include_once( '".st."file".et."' );".st.et
exec "Snippet incl include( '".st."file".et."' );".st.et
exec "Snippet foreach foreach( $".st."variable".et." as $".st."key".et." => $".st."value".et." )<CR>{<CR>".st.et."<CR>}<CR>".st.et
exec "Snippet ifelse if ( ".st."condition".et." )<CR>{<CR>".st.et."<CR>}<CR>else<CR>{<CR>".st.et."<CR>}<CR>".st.et
exec "Snippet $_ $_REQUEST['".st."variable".et."']<CR>".st.et
exec "Snippet case case '".st."variable".et."':<CR>".st.et."<CR>break;<CR>".st.et
exec "Snippet print print \"".st."string".et."\"".st.et.";".st.et."<CR>".st.et
exec "Snippet function ".st."public".et."function ".st."FunctionName".et." (".st.et.")<CR>{<CR>".st.et."<CR>}<CR>".st.et
exec "Snippet if if ( ".st."condition".et." )<CR>{<CR>".st.et."<CR>}<CR>".st.et
exec "Snippet else else<CR>{<CR>".st.et."<CR>}<CR>".st.et
exec "Snippet array $".st."arrayName".et." = array( '".st.et."',".st.et." );".st.et
exec "Snippet -globals $GLOBALS['".st."variable".et."']".st.et.st."something".et.st.et.";<CR>".st.et
exec "Snippet req require( '".st."file".et."' );<CR>".st.et
exec "Snippet for for ( $".st."i".et."=".st.et."; $".st."i".et." < ".st.et."; $".st."i".et."++ )<CR>{ <CR>".st.et."<CR>}<CR>".st.et
exec "Snippet while while ( ".st.et." )<CR>{<CR>".st.et."<CR>}<CR>".st.et
after/ftplugin/phpdoc_snippets.vim
19
if !exists('loaded_snippet') || &cp
    finish
endif

let st = g:snip_start_tag
let et = g:snip_end_tag
let cd = g:snip_elem_delim

exec "Snippet doc_d /**<CR>* ".st."undocumentedConstant".et."<CR>**/<CR>define(".st.et.", ".st.et.");".st.et."<CR>".st.et
exec "Snippet doc_vp /**<CR>* ".st."undocumentedClassVariable".et."<CR>*<CR>* @var ".st."string".et.st.et."<CR>**/".st.et."<CR>"
exec "Snippet doc_f /**<CR>* ".st."undocumentedFunction".et."<CR>*<CR>* @return ".st."void".et."<CR>* @author ".st.et."<CR>**/<CR>".st.et."function ".st.et."(".st.et.")<CR>{".st.et."<CR>}<CR>".st.et
exec "Snippet doc_s /**<CR>* ".st."undocumentedFunction".et."<CR>*<CR>* @return ".st."void".et."<CR>* @author ".st.et."<CR>**/<CR>".st.et."function ".st.et."(".st.et.");<CR>".st.et
exec "Snippet doc_h /**<CR>* ".st.et."<CR>*<CR>* @author ".st.et."<CR>* @version $Id$<CR>* @copyright ".st.et.", ".st.et."<CR>* @package ".st."default".et."<CR>**/<CR><CR>/**<CR>* Define DocBlock<CR>**/<CR><CR>".st.et
exec "Snippet doc_fp /**<CR>* ".st."undocumentedFunction".et."<CR>*<CR>* @return ".st."void".et."<CR>* @author ".st.et."<CR>**/".st.et."<CR>"
exec "Snippet doc_i /**<CR>* ".st."undocumentedClass".et."<CR>*<CR>* @package ".st."default".et."<CR>* @author ".st.et."<CR>**/<CR>interface ".st.et."<CR>{".st.et."<CR>} // END interface ".st.et."<CR>".st.et
exec "Snippet doc_fp /**<CR>* ".st."undocumentedConstant".et.st.et."<CR>**/".st.et."<CR>".st.et
exec "Snippet doc_v /**<CR>* ".st."undocumentedClassVariable".et."<CR>*<CR>* @var ".st."string".et."<CR>**/<CR><var> $".st.et.";".st.et."<CR>".st.et
exec "Snippet doc_cp /**<CR>* ".st."undocumentedClass".et."<CR>*<CR>* @package ".st."default".et."<CR>* @author ".st.et."<CR>**/".st.et
exec "Snippet doc_c /**<CR>* ".st."undocumentedClass".et."<CR>*<CR>* @package ".st."default".et."<CR>* @author ".st.et."<CR>**/<CR>".st."class".et."class ".st."a".et."<CR>{".st.et."<CR>} // END ".st."class".et."class ".st."a".et."<CR>".st.et
after/ftplugin/propel_snippets.vim
14
if !exists('loaded_snippet') || &cp
    finish
endif

let st = g:snip_start_tag
let et = g:snip_end_tag
let cd = g:snip_elem_delim

exec "Snippet <i <index name=\"".st."key".et."_index\"><CR><index-column name=\"".st."key".et."\" /><CR></index><CR>".st.et
exec "Snippet <t <table name=\"".st."name".et."\" ".st.et."><CR>".st.et."<CR></table><CR>".st.et
exec "Snippet <u <unique name=\"unique_".st."key".et."\"><CR><unique-column name=\"".st."key".et."\" /><CR></unique><CR>".st.et
exec "Snippet <c <column name=\"".st."name".et."\" type=\"".st."type".et."\" ".st.et." /><CR>".st.et
exec "Snippet <p <column name=\"".st."id".et."\" type=\"".st."integer".et."\" required=\"true\" primaryKey=\"true\" autoincrement=\"true\" /><CR>".st.et
exec "Snippet <f <foreign-key foreignTable=\"".st."table".et."\"><CR><reference local=\"".st."table".et."_id\" foreign=\"<id>\"/><CR></foreign-key><CR>".st.et
after/ftplugin/python_snippets.vim
202
if !exists('loaded_snippet') || &cp
    finish
endif

" Given a string containing a list of arguments (e.g. "one, two = 'test'"),
" this function cleans it up by removing useless whitespace and commas.
function! PyCleanupArgs(text)
    if a:text == 'args'
        return ''
    endif
    let text = substitute(a:text, '\(\w\)\s\(\w\)', '\1,\2', 'g')
    return join(split(text, '\s*,\s*'), ', ')
endfunction

" Given a string containing a list of arguments (e.g. "one = 'test', *args,
" **kwargs"), this function returns a string containing only the variable
" names, separated by spaces, e.g. "one two".
function! PyGetVarnamesFromArgs(text)
    let text = substitute(a:text, 'self,*\s*', '',  '')
    let text = substitute(text, '\*\*\?\k\+', '',  'g')
    let text = substitute(text,   '=.\{-},',    '',  'g')
    let text = substitute(text,   '=.\{-}$',    '',  'g')
    let text = substitute(text,   '\s*,\s*',    ' ', 'g')
    if text == ' '
        return ''
    endif
    return text
endfunction

" Returns the current indent as a string.
function! PyGetIndentString()
    if &expandtab
        let tabs   = indent('.') / &shiftwidth
        let tabstr = repeat(' ', &shiftwidth)
    else
        let tabs   = indent('.') / &tabstop
        let tabstr = '\t'
    endif
    return repeat(tabstr, tabs)
endfunction

" Given a string containing a list of arguments (e.g. "one = 'test', *args,
" **kwargs"), this function returns them formatted correctly for the
" docstring.
function! PyGetDocstringFromArgs(text)
    let text = PyGetVarnamesFromArgs(a:text)
    if a:text == 'args' || text == ''
        return ''
    endif
    let indent  = PyGetIndentString()
    let st      = g:snip_start_tag
    let et      = g:snip_end_tag
    let docvars = map(split(text), 'v:val." -- ".st.et')
    return '\n'.indent.join(docvars, '\n'.indent).'\n'.indent
endfunction

" Given a string containing a list of arguments (e.g. "one = 'test', *args,
" **kwargs"), this function returns them formatted as a variable assignment in
" the form "self._ONE = ONE", as used in class constructors.
function! PyGetVariableInitializationFromVars(text)
    let text = PyGetVarnamesFromArgs(a:text)
    if a:text == 'args' || text == ''
        return ''
    endif
    let indent      = PyGetIndentString()
    let st          = g:snip_start_tag
    let et          = g:snip_end_tag
    let assert_vars = map(split(text), '"assert ".v:val." ".st.et')
    let assign_vars = map(split(text), '"self._".v:val." = ".v:val')
    let assertions  = join(assert_vars, '\n'.indent)
    let assignments = join(assign_vars, '\n'.indent)
    return assertions.'\n'.indent.assignments.'\n'.indent
endfunction

" Given a string containing a list of arguments (e.g. "one = 'test', *args,
" **kwargs"), this function returns them with the default arguments removed.
function! PyStripDefaultValue(text)
    return substitute(a:text, '=.*', '', 'g')
endfunction

" Returns the number of occurences of needle in haystack.
function! Count(haystack, needle)
    let counter = 0
    let index = match(a:haystack, a:needle)
    while index > -1
        let counter = counter + 1
        let index = match(a:haystack, a:needle, index+1)
    endwhile
    return counter
endfunction

" Returns replacement if the given subject matches the given match.
" Returns the subject otherwise.
function! PyReplace(subject, match, replacement)
    if a:subject == a:match
        return a:replacement
    endif
    return a:subject
endfunction

" Returns the % operator with a tuple containing n elements appended, where n
" is the given number.
function! PyHashArgList(count)
    if a:count == 0
        return ''
    endif
    let st = g:snip_start_tag
    let et = g:snip_end_tag
    return ' % ('.st.et.repeat(', '.st.et, a:count - 1).')'
endfunction

let st = g:snip_start_tag
let et = g:snip_end_tag
let cd = g:snip_elem_delim

" Note to users: The following method of defininf snippets is to allow for
" changes to the default tags.
" Feel free to define your own as so:
"    Snippet mysnip This is the expansion text.<{}>
" There is no need to use exec if you are happy to hardcode your own start and
" end tags

" Properties, setters and getters.
exec "Snippet prop ".st."attribute".et." = property(get_".st."attribute".et.", set_".st."attribute".et.st.et.")<CR>".st.et
exec "Snippet get def get_".st."name".et."(self):<CR>return self._".st."name".et."<CR>".st.et
exec "Snippet set def set_".st."name".et."(self, ".st."value".et."):
\<CR>self._".st."name".et." = ".st."value:PyStripDefaultValue(@z)".et."
\<CR>".st.et

" Functions and methods.
exec "Snippet def def ".st."fname".et."(".st."args:PyCleanupArgs(@z)".et."):
\<CR>\"\"\"
\<CR>".st.et."
\<CR>".st."args:PyGetDocstringFromArgs(@z)".et."\"\"\"
\<CR>".st."pass".et."
\<CR>".st.et
exec "Snippet cm ".st."class".et." = classmethod(".st."class".et.")<CR>".st.et

" Class definition.
exec "Snippet cl class ".st."ClassName".et."(".st."object".et."):
\<CR>\"\"\"
\<CR>This class represents ".st.et."
\<CR>\"\"\"
\<CR>
\<CR>def __init__(self, ".st."args:PyCleanupArgs(@z)".et."):
\<CR>\"\"\"
\<CR>Constructor.
\<CR>".st."args:PyGetDocstringFromArgs(@z)".et."\"\"\"
\<CR>".st."args:PyGetVariableInitializationFromVars(@z)".et.st.et

" Keywords
exec "Snippet for for ".st."variable".et." in ".st."ensemble".et.":<CR>".st."pass".et."<CR>".st.et
exec "Snippet pf print '".st."s".et."'".st."s:PyHashArgList(Count(@z, '%[^%]'))".et."<CR>".st.et
exec "Snippet im import ".st."module".et."<CR>".st.et
exec "Snippet from from ".st."module".et." import ".st.'name:PyReplace(@z, "name", "*")'.et."<CR>".st.et
exec "Snippet % '".st."s".et."'".st."s:PyHashArgList(Count(@z, '%[^%]'))".et.st.et
exec "Snippet ass assert ".st."expression".et.st.et
" From Kib2
exec "Snippet bc \"\"\"<CR>".st.et."<CR>\"\"\"<CR>".st.et

" Try, except, finally.
exec "Snippet trye try:
\<CR>".st.et."
\<CR>except Exception, e:
\<CR>".st.et."
\<CR>".st.et

exec "Snippet tryf try:
\<CR>".st.et."
\<CR>finally:
\<CR>".st.et."
\<CR>".st.et

exec "Snippet tryef try:
\<CR>".st.et."
\<CR>except Exception, e:
\<CR>".st.et."
\<CR>finally:
\<CR>".st.et."
\<CR>".st.et

" Other multi statement templates
" From Panos
exec "Snippet ifn if __name__ == '".st."main".et."':<CR>".st.et
exec "Snippet ifmain if __name__ == '__main__':<CR>".st.et

" Shebang
exec "Snippet sb #!/usr/bin/env python<CR># -*- coding: ".st."encoding".et." -*-<CR>".st.et
exec "Snippet sbu #!/usr/bin/env python<CR># -*- coding: UTF-8 -*-<CR>".st.et
" From Kib2
exec "Snippet sbl1 #!/usr/bin/env python<CR># -*- coding: Latin-1 -*-<CR>".st.et

" Unit tests.
exec "Snippet unittest if __name__ == '__main__':
\<CR>import unittest
\<CR>
\<CR>class ".st."ClassName".et."Test(unittest.TestCase):
\<CR>def setUp(self):
\<CR>".st."pass".et."
\<CR>
\<CR>def runTest(self):
\<CR>".st.et
after/ftplugin/rails_snippets.vim
54
if !exists('loaded_snippet') || &cp
    finish
endif

let st = g:snip_start_tag
let et = g:snip_end_tag
let cd = g:snip_elem_delim

exec "Snippet mrnt rename_table \"".st."oldTableName".et."\", \"".st."newTableName".et."\"".st.et
exec "Snippet rfu render :file => \"".st."filepath".et."\", :use_full_path => ".st."false".et.st.et
exec "Snippet rns render :nothing => ".st."true".et.", :status => ".st.et.st.et
exec "Snippet ri render :inline => \"".st.et."\")>\"".st.et
exec "Snippet rt render :text => \"".st.et."\"".st.et
exec "Snippet mcc t.column \"".st."title".et."\", :".st."string".et.st.et
exec "Snippet rpl render :partial => \"".st."item".et."\", :locals => { :".st."name".et." => \"".st."value".et."\"".st.et." }".st.et
exec "Snippet rea redirect_to :action => \"".st."index".et."\"".st.et
exec "Snippet rtlt render :text => \"".st.et."\", :layout => ".st."true".et.st.et
exec "Snippet ft <%= form_tag :action => \"".st."update".et."\" %>".st.et
exec "Snippet forin <% for ".st."item".et." in ".st.et." %><CR><Tab><%= ".st."item".et.".".st."name".et." %><CR><% end %><CR>".st.et
exec "Snippet lia <%= link_to \"".st.et."\", :action => \"".st."index".et."\" %>".st.et
exec "Snippet rl render :layout => \"".st."layoutname".et."\"".st.et
exec "Snippet ra render :action => \"".st."action".et."\"".st.et
exec "Snippet mrnc rename_column \"".st."table".et."\", \"".st."oldColumnName".et."\", \"".st."newColumnName".et."\"".st.et
exec "Snippet mac add_column \"".st."table".et."\", \"".st."column".et."\", :".st."string".et.st.et
exec "Snippet rpc render :partial => \"".st."item".et."\", :collection => ".st."items".et.st.et
exec "Snippet rec redirect_to :controller => \"".st."items".et."\"".st.et
exec "Snippet rn render :nothing => ".st."true".et.st.et
exec "Snippet lic <%= link_to \"".st.et."\", :controller => \"".st.et."\" %>".st.et
exec "Snippet rpo render :partial => \"".st."item".et."\", :object => ".st."object".et.st.et
exec "Snippet rts render :text => \"".st.et."\", :status => ".st.et
exec "Snippet rcea render_component :action => \"".st."index".et."\"".st.et
exec "Snippet recai redirect_to :controller => \"".st."items".et."\", :action => \"".st."show".et."\", :id => ".st.et
exec "Snippet mcdt create_table \"".st."table".et."\" do |t|<CR><Tab>".st.et."<CR>end<CR>".st.et
exec "Snippet ral render :action => \"".st."action".et."\", :layout => \"".st."layoutname".et."\"".st.et
exec "Snippet rit render :inline => \"".st.et."\", :type => ".st.et
exec "Snippet rceca render_component :controller => \"".st."items".et."\", :action => \"".st."index".et."\"".st.et
exec "Snippet licai <%= link_to \"".st.et."\", :controller => \"".st."items".et."\", :action => \"".st."edit".et."\", :id => ".st.et." %>".st.et
exec "Snippet verify verify :only => [:".st.et."], :method => :post, :render => {:status => 500, :text => \"use HTTP-POST\"}".st.et
exec "Snippet mdt drop_table \"".st."table".et."\"".st.et
exec "Snippet rp render :partial => \"".st."item".et."\"".st.et
exec "Snippet rcec render_component :controller => \"".st."items".et."\"".st.et
exec "Snippet mrc remove_column \"".st."table".et."\", \"".st."column".et."\"".st.et
exec "Snippet mct create_table \"".st."table".et."\" do |t|<CR><Tab>".st.et."<CR>end<CR>".st.et
exec "Snippet flash flash[:".st."notice".et."] = \"".st.et."\"".st.et
exec "Snippet rf render :file => \"".st."filepath".et."\"".st.et
exec "Snippet lica <%= link_to \"".st.et."\", :controller => \"".st."items".et."\", :action => \"".st."index".et."\" %>".st.et
exec "Snippet liai <%= link_to \"".st.et."\", :action => \"".st."edit".et."\", :id => ".st.et." %>".st.et
exec "Snippet reai redirect_to :action => \"".st."show".et."\", :id => ".st.et
exec "Snippet logi logger.info \"".st.et."\"".st.et
exec "Snippet marc add_column \"".st."table".et."\", \"".st."column".et."\", :".st."string".et."<CR><CR>".st.et."<CR>".st.et
exec "Snippet rps render :partial => \"".st."item".et."\", :status => ".st.et
exec "Snippet ril render :inline => \"".st.et."\", :locals => { ".st.et." => \"".st."value".et."\"".st.et." }".st.et
exec "Snippet rtl render :text => \"".st.et."\", :layout => \"".st.et."\"".st.et
exec "Snippet reca redirect_to :controller => \"".st."items".et."\", :action => \"".st."list".et."\"".st.et
after/ftplugin/ruby_snippets.vim
32
if !exists('loaded_snippet') || &cp
    finish
endif

let st = g:snip_start_tag
let et = g:snip_end_tag
let cd = g:snip_elem_delim

exec "Snippet do do<CR>".st.et."<CR>end<CR>".st.et
exec "Snippet class class ".st."className".et."<CR>".st.et."<CR>end<CR>".st.et
exec "Snippet begin begin<CR>".st.et."<CR>rescue ".st."Exception".et." => ".st."e".et."<CR>".st.et."<CR>end<CR>".st.et
exec "Snippet each_with_index0 each_with_index do |".st."element".et.", ".st."index".et."|<CR>".st."element".et.".".st.et."<CR>end<CR>".st.et
exec "Snippet collect collect { |".st."element".et."| ".st."element".et.".".st.et." }<CR>".st.et
exec "Snippet forin for ".st."element".et." in ".st."collection".et."<CR>".st."element".et.".".st.et."<CR>end<CR>".st.et
exec "Snippet doo do |".st."object".et."|<CR>".st.et."<CR>end<CR>".st.et
exec "Snippet : :".st."key".et." => \"".st."value".et."\"".st.et."<CR>".st.et
exec "Snippet def def ".st."methodName".et."<CR>".st.et."<CR>end<CR>".st.et
exec "Snippet case case ".st."object".et."<CR>when ".st."condition".et."<CR>".st.et."<CR>end<CR>".st.et
exec "Snippet collecto collect do |".st."element".et."|<CR>".st."element".et.".".st.et."<CR>end<CR>".st.et
exec "Snippet each each { |".st."element".et."| ".st."element".et.".".st.et." }<CR>".st.et
exec "Snippet each_with_index each_with_index { |".st."element".et.", ".st."idx".et."| ".st."element".et.".".st.et." }<CR>".st.et
exec "Snippet if if ".st."condition".et."<CR>".st.et."<CR>end<CR>".st.et
exec "Snippet eacho each do |".st."element".et."|<CR>".st."element".et.".".st.et."<CR>end<CR>".st.et
exec "Snippet unless unless ".st."condition".et."<CR>".st.et."<CR>end<CR>".st.et
exec "Snippet ife if ".st."condition".et."<CR>".st.et."<CR>else<CR>".st.et."<CR>end<CR>".st.et
exec "Snippet when when ".st."condition".et."<CR>".st.et
exec "Snippet selecto select do |".st."element".et."|<CR>".st."element".et.".".st.et."<CR>end<CR>".st.et
exec "Snippet injecto inject(".st."object".et.") do |".st."injection".et.", ".st."element".et."| <CR>".st.et."<CR>end<CR>".st.et
exec "Snippet reject { |".st."element".et."| ".st."element".et.".".st.et." }<CR>".st.et
exec "Snippet rejecto reject do |".st."element".et."| <CR>".st."element".et.".".st.et."<CR>end<CR>".st.et
exec "Snippet inject inject(".st."object".et.") { |".st."injection".et.", ".st."element".et."| ".st.et." }<CR>".st.et
exec "Snippet select select { |".st."element".et."| ".st."element".et.".".st.et." }<CR>".st.et
after/ftplugin/sh_snippets.vim
12
if !exists('loaded_snippet') || &cp
    finish
endif

let st = g:snip_start_tag
let et = g:snip_end_tag
let cd = g:snip_elem_delim

"Snippet !env #!/usr/bin/env ${1:${TM_SCOPE/(?:source|.*)\\.(\\w+).*/$1/}}
exec "Snippet if if [[ ".st."condition".et." ]]; then<CR>".st.et."<CR>fi".st.et
exec "Snippet elif elif [[ ".st."condition".et." ]]; then<CR>".st.et
exec "Snippet for for (( ".st."i".et." = ".st.et."; ".st."i".et." ".st.et."; ".st."i".et.st.et." )); do<CR>".st.et."<CR>done".st.et
after/ftplugin/slate_snippets.vim
19
if !exists('loaded_snippet') || &cp
    finish
endif

let st = g:snip_start_tag
let et = g:snip_end_tag
let cd = g:snip_elem_delim

exec "Snippet do do: [| :".st."each".et."| ".st.et."]<CR>".st.et
exec "Snippet proto define: #".st."NewName".et." &parents: {".st."parents".et."} &slots: {".st."slotSpecs".et."}.<CR>".st.et
exec "Snippet ifte ".st."condition".et." ifTrue: [".st.et.":then] ifFalse: [".st.et.":else]<CR>".st.et
exec "Snippet collect collect: [| :".st."each".et."| ".st.et."]<CR>".st.et
exec "Snippet if ".st."condition".et." ifTrue: [".st.et.":then]".st.et
exec "Snippet until [".st."condition".et."] whileFalse: [".st.et.":body]".st.et
exec "Snippet reject reject: [| :".st."each".et."| ".st.et."]<CR>".st.et
exec "Snippet dowith doWithIndex: [| :".st."each".et." :".st."index".et." | ".st.et."]<CR>".st.et
exec "Snippet select select: [| :".st."each".et."| ".st.et."]".st.et
exec "Snippet while [".st."condition".et."] whileTrue: [".st.et.":body]".st.et
exec "Snippet inject inject: ".st."object".et." [| :".st."injection".et.", :".st."each".et."| ".st.et."]".st.et
after/ftplugin/smarty_snippets.vim
35
if !exists('loaded_snippet') || &cp
    finish
endif

let st = g:snip_start_tag
let et = g:snip_end_tag
let cd = g:snip_elem_delim

exec "Snippet {cycle {cycle values=\"#SELSTART#".st."foo".et.",".st."bar".et."#SELEND#\" name=\"default\" print=true advance=true delimiter=\",\" assign=varname }<CR>".st.et
exec "Snippet |regex_replace |regex_replace:\"".st."regex".et."\":\"".st.et."\"".st.et
exec "Snippet {counter {counter name=\"#INSERTION#\" start=1 skip=1 direction=\"up\" print=true<CR>assign=\"foo\" }<CR><CR>{counter}<CR>".st.et
exec "Snippet {eval {eval var=\"#SELSTART#{template_format}#SELEND#\" assign=varname} <CR>".st.et
"Snippet |date_format |date_format:"${1:strftime() formatting}" <CR><{}>
exec "Snippet |truncate |truncate:".st.et.":".st.et.":".st."false".et.""
exec "Snippet {if {if ".st."varname".et.st.et."<CR>\"".st."foo".et."\"}<CR><CR>{* $varname can also be a php call *}<CR><CR>".st.et."<CR><CR>{/if}<CR>".st.et
"Snippet |string_format |string_format:"${1:sprintf formatting}" <CR><{}>
exec "Snippet {assign {assign var=".st.et." value=\"".st.et."\"}".st.et
exec "Snippet {foreach {foreach from=".st."varname".et." item=i [key=k name=\"\"] }<CR><CR>".st.et."<CR><CR>{/foreach}<CR><CR>".st.et
exec "Snippet {capture {capture name=#INSERTION#}<CR><CR>#SELECT#<CR><CR>{/capture}<CR>".st.et
exec "Snippet |wordwrap |wordwrap:".st.et.":\"".st.et."\":".st.et
exec "Snippet |spacify |spacify:\"".st.et."\"".st.et." "
exec "Snippet |default |default:\"".st.et."\"".st.et
exec "Snippet {debug {debug output=\"#SELSTART#".st.et."#SELEND#\" }".st.et
exec "Snippet |replace |replace:\"".st."needle".et."\":\"".st.et."\"".st.et
exec "Snippet {include {include file=\"".st.et."\" [assign=varname foo=\"bar\"] }".st.et
exec "Snippet |escape |escape:\"".st.et."\"".st.et
exec "Snippet {strip {strip}<CR>".st.et."<CR>{/strip}".st.et
exec "Snippet {math {math equation=\"".st.et."\" assign=".st.et." ".st.et."}".st.et
exec "Snippet {config_load {config_load file=\"#INSERTION#\" [section=\"\" scope=\"local|parent|global\"] }".st.et
exec "Snippet |cat  |cat:\"".st.et."\"".st.et
exec "Snippet {insert {insert name=\"insert_".st.et."\" [assign=varname script=\"foo.php\" foo=\"bar\"] }".st.et
exec "Snippet {fetch {fetch file=\"#SELSTART#http:// or file#SELEND#\" assign=varname}".st.et
exec "Snippet {literal {literal}<CR><CR>".st.et."<CR><CR>{/literal}".st.et
exec "Snippet {include_php {include_php file=\"".st.et."\" [once=true]}".st.et
exec "Snippet |strip |strip:[\"".st.et."\"]".st.et
after/ftplugin/symfony_snippets.vim
21
if !exists('loaded_snippet') || &cp
    finish
endif

let st = g:snip_start_tag
let et = g:snip_end_tag
let cd = g:snip_elem_delim

exec "Snippet image_tag image_tag('".st."imageName".et."'".st.et.")".st.et
exec "Snippet get public function get".st.et." ()<CR>{<CR>return $this->".st.et.";<CR>}<CR><CR>".st.et
exec "Snippet link_to link_to('".st."linkName".et."', '".st."moduleName".et."/".st."actionName".et.st.et."')".st.et
exec "Snippet sexecute public function execute<Action>()<CR>{<CR>".st.et."<CR>}<CR>".st.et
exec "Snippet set public function set".st.et." ($".st.et.")<CR>{<CR>$this->".st.et." = ".st.et.";<CR>}<CR><CR>".st.et
exec "Snippet execute /**<CR>* ".st."className".et."<CR>*<CR>*/<CR>public function execute<Action>()<CR>{<CR>".st.et."<CR>}<CR>".st.et
exec "Snippet tforeach <?php foreach ($".st."variable".et." as $".st."key".et.st.et."): ?><CR>".st.et."<CR><?php endforeach ?><CR>".st.et
exec "Snippet getparam $this->getRequestParameter('".st."id".et."')".st.et
exec "Snippet div <div".st.et."><CR>".st.et."<CR></div>".st.et
exec "Snippet tif <?php if (".st."condition".et."): ?><CR>".st.et."<CR><?php endif ?><CR>".st.et
exec "Snippet setget public function set".st."var".et." (".st."arg".et.")<CR>{<CR>$this->".st."arg".et." = ".st."arg".et.";<CR>}<CR><CR>public function get".st."var".et." ()<CR>{<CR>return $this->".st."var".et.";<CR>}<CR><CR>".st.et
exec "Snippet echo <?php echo ".st.et." ?>".st.et
exec "Snippet tfor <?php for($".st."i".et." = ".st.et."; $".st."i".et." <= ".st.et."; $".st."i".et."++): ?><CR>".st.et."<CR><?php endfor ?><CR>".st.et
after/ftplugin/tcl_snippets.vim
14
if !exists('loaded_snippet') || &cp
    finish
endif

let st = g:snip_start_tag
let et = g:snip_end_tag
let cd = g:snip_elem_delim

exec "Snippet switch switch ".st.et." -- $".st."var".et." {<CR>".st."match".et." {<CR>".st.et."<CR>}<CR>default<CR>{".st.et."}<CR>}<CR>".st.et
exec "Snippet foreach foreach ".st."var".et." $".st."list".et." {<CR>".st.et."<CR>}<CR>".st.et
exec "Snippet proc proc ".st."name".et." {".st."args".et."} <CR>{<CR>".st.et."<CR>}<CR>".st.et
exec "Snippet if if {".st."condition".et."} {<CR>".st.et."<CR>}<CR>".st.et
exec "Snippet for for {".st."i".et." {".st.et."} {".st.et."} {<CR>".st.et."<CR>}<CR>".st.et
exec "Snippet while while {".st."condition".et."} {<CR>".st.et."<CR>}<CR>".st.et
after/ftplugin/template_toolkit_snippets.vim
13
if !exists('loaded_snippet') || &cp
    finish
endif

let st = g:snip_start_tag
let et = g:snip_end_tag
let cd = g:snip_elem_delim

exec "Snippet wrap [% WRAPPER ".st."template".et." %]<CR>".st.et."<CR>[% END %]<CR>".st.et
exec "Snippet if [% IF ".st."condition".et." %]<CR>".st.et."<CR>[% ELSE %]<CR>".st.et."<CR>[% END %]<CR>".st.et
exec "Snippet unl [% UNLESS ".st."condition".et." %]<CR>".st.et."<CR>[% END %]<CR>".st.et
exec "Snippet inc [% INCLUDE ".st."template".et." %]<CR>".st.et
exec "Snippet for  [% FOR ".st."var".et." IN ".st."set".et." %]<CR>".st.et."<CR>[% END %]".st.et
after/ftplugin/tex_snippets.vim
13
if !exists('loaded_snippet') || &cp
    finish
endif

let st = g:snip_start_tag
let et = g:snip_end_tag
let cd = g:snip_elem_delim

exec "Snippet sub \\subsection{".st."name".et."}\\label{sub:".st."name:substitute(@z,'.','\\l&','g')".et."}<CR>".st.et
exec "Snippet $$ \\[<CR>".st.et."<CR>\\]<CR>".st.et
exec "Snippet ssub \\subsubsection{".st."name".et."}\\label{ssub:".st."name:substitute(@z,'.','\\l&','g')".et."}<CR>".st.et
exec "Snippet itd \\item[".st."desc".et."] ".st.et
exec "Snippet sec \\section{".st."name".et."}\\label{sec:".st."name:substitute(@z,'.','\\l&','g')".et."<CR>".st.et
after/ftplugin/xhtml_snippets.vim
48
if !exists('loaded_snippet') || &cp
    finish
endif

let st = g:snip_start_tag
let et = g:snip_end_tag
let cd = g:snip_elem_delim

exec "Snippet doctype <!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"<CR>\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\"><CR>".st.et
exec "Snippet aref <a href=\"".st.et."\" id=\"".st.et."\" title=\"".st.et."\">".st.et."</a>".st.et
exec "Snippet head  <head><CR>".st.et."<CR></head>".st.et
exec "Snippet script <script type=\"text/javascript\" language=\"<javascript>\" charset=\"".st.et."\"><CR>// <![CDATA[<CR>".st.et."<CR>// ]]><CR></script>".st.et
exec "Snippet html <html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"".st."en".et."\"<CR>lang=\"".st."en".et."\"><CR>".st.et."<CR></html>"
exec "Snippet h3 <h3>".st.et."</h3>".st.et
exec "Snippet h4 <h4>".st.et."</h4>".st.et
exec "Snippet h5 <h5>".st.et."</h5>".st.et
exec "Snippet h6 <h6>".st.et."</h6>".st.et
exec "Snippet fieldset <fieldset><CR>".st.et."<CR></fieldset>".st.et
exec "Snippet noscript <noscript><CR>".st.et."<CR></noscript>".st.et
exec "Snippet ul <ul ".st.et."><CR>".st.et."<CR></ul>".st.et
exec "Snippet xml <?xml version=\"1.0\" encoding=\"iso-8859-1\"?><CR><CR>".st.et
exec "Snippet body <body id=\"".st.et."\" ".st.et."><CR>".st.et."<CR></body>".st.et
exec "Snippet legend <legend align=\"".st.et."\" accesskey=\"".st.et."\"><CR>".st.et."<CR></legend>".st.et
exec "Snippet title <title>".st."PageTitle".et."</title>".st.et
exec "Snippet scriptsrc <script src=\"".st.et."\" type=\"text/javascript\" language=\"<javascript>\" charset=\"".st.et."\"></script>".st.et
exec "Snippet img <img src=\"".st.et."\" alt=\"".st.et."\" class=\"".st.et."\" />".st.et
exec "Snippet option <option label=\"".st."label".et."\" value=\"".st."value".et."\" ".st.et."></option> ".st.et
exec "Snippet optgroup <optgroup label=\"".st."Label".et."\"><CR>".st.et."<CR></optgroup>".st.et
exec "Snippet meta <meta name=\"".st."name".et."\" content=\"".st."content".et."\" />".st.et
exec "Snippet td <td ".st.et.">".st.et."</td>".st.et
exec "Snippet dt <dt>".st.et."<CR></dt><CR><dd>".st.et."</dd>".st.et
exec "Snippet tfoot <tfoot><CR>".st.et."<CR></tfoot>".st.et
exec "Snippet div <!-- begin div.".st."id".et." --><CR><div id=\"".st."id".et."\"><CR>".st.et."<CR></div><CR><!-- end div.".st."id".et." --><CR>".st.et
exec "Snippet ol <ol ".st.et."><CR>".st.et."<CR></ol>".st.et
exec "Snippet txtarea <textarea id=\"".st."ID".et."\" name=\"".st."Name".et."\" rows=\"".st.et."\" cols=\"".st.et."\" tabindex=\"".st.et."\" ".st.et.">".st.et."</textarea>".st.et
exec "Snippet mailto <a href=\"mailto:".st.et."?subject=".st.et."\">".st.et."</a>".st.et
exec "Snippet table <table summary=\"".st."Summary".et."\" class=\"".st."className".et."\" width=\"".st.et."\" cellspacing=\"".st.et."\" cellpadding=\"".st.et."\" border=\"".st.et."\"><CR>".st.et."<CR></table>".st.et
exec "Snippet hint <span class=\"hint\">".st.et."</span>".st.et
exec "Snippet link <link rel=\"".st."stylesheet".et."\" href=\"".st.et."\" type=\"text/css\" media=\"".st."screen".et."\" title=\"".st.et."\" charset=\"".st.et."\" />".st.et
exec "Snippet form <form action=\"".st."urlToGoTo".et."\" method=\"".st."get".et."\" id=\"".st."formID".et."\" name=\"".st."formName".et."\"><CR>".st.et."<CR></form>".st.et
exec "Snippet tr <tr ".st.et."><CR>".st.et."<CR></tr>".st.et
exec "Snippet label <label for=\"".st."inputItem".et."\">".st.et."</label>".st.et
exec "Snippet image <img src=\"".st.et."\" alt=\"".st.et."\" width=\"".st.et."\" height=\"".st.et."\" ".st.et."/>".st.et
exec "Snippet input <input name=\"".st.et."\" id=\"".st.et."\" type=\"radio\" value=\"".st."defaultValue".et."\" tabindex=\"".st.et."\" ".st.et." />".st.et
exec "Snippet select <select id=\"".st."ID".et."\" name=\"".st."Name".et."\" size=\"".st.et."\" tabindex=\"".st.et."\" ".st.et."><CR>".st.et."<CR></select><CR>".st.et
exec "Snippet style <style type=\"text/css\" media=\"".st."screen".et."\"><CR>/* <![CDATA[ */<CR>".st.et."<CR>/* ]]> */<CR></style><CR>".st.et
exec "Snippet divheader <!-- Begin HeaderDiv:: --><CR><div id=\"HeaderDiv\"><CR><!--logo in background --><CR><h1>".st."CompanyName".et."</h1><CR></div><CR><!-- End HeaderDiv:: --><CR>".st.et
exec "Snippet base <base href=\"".st.et."\" ".st.et."/>".st.et
