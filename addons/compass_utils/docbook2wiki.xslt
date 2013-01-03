<?xml version="1.0" encoding="utf-8"?><!-- -*- coding: utf-8;-*- -->
<!DOCTYPE xsl:stylesheet [
<!ENTITY tab "<xsl:text>    </xsl:text>"> <!-- Tab -->
<!ENTITY n "<xsl:text>---NEWLINE---</xsl:text>"> <!-- New Line -->
]>

<xsl:stylesheet version="1.0"
xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
<xsl:output method = "text" indent = "no" />

  <xsl:param name='base_path' select='/'/>
  
<!-- ################################################################################ -->
<!-- #    functions -->
<!-- ################################################################################ -->

    
  <!-- template that does a search & replace -->
  <!-- replace-list contains pairs of replace-from,replace-to -->
  <!-- all fields in replace-list separated by character $sep -->

  <xsl:template name="replace-text-list">
    <xsl:param name="text"/>
    <xsl:param name="replace-list"/>
    <xsl:param name="sep" select="','"/>
    
    <!-- these params are only used internally -->
    <xsl:param name="replace-list-rest" select="$replace-list"/>
    <xsl:param name="replace-best"/>
    <xsl:param name="by-best"/>
    <xsl:param name="len-best" select="-1"/>
    
    <xsl:choose>
      <xsl:when test="contains($replace-list-rest,$sep)">
	      <xsl:variable name="replace" select="substring-before($replace-list-rest,$sep)"/>
	      <xsl:variable name="rest1" select="substring-after($replace-list-rest,$sep)"/>
	      <xsl:variable name="by">
	        <xsl:choose>
	          <xsl:when test="contains($rest1,$sep)">
	            <xsl:value-of select="substring-before($rest1,$sep)"/>
	          </xsl:when>
	          <xsl:otherwise>
	            <xsl:value-of select="$rest1"/>
	          </xsl:otherwise>
	        </xsl:choose>
	      </xsl:variable>   
	      <xsl:variable name="rest">
	        <xsl:choose>
	          <xsl:when test="contains($rest1,$sep)">
	            <xsl:value-of select="substring-after($rest1,$sep)"/>
	          </xsl:when>
	          <xsl:otherwise>
	            <xsl:value-of select="''"/>
	          </xsl:otherwise>
	        </xsl:choose>
	      </xsl:variable>   
	      <xsl:variable name="len">
	        <xsl:choose>
	          <xsl:when test="contains($text,$replace)">
	            <xsl:value-of select="string-length(substring-before($text,$replace))"/>
	          </xsl:when>
	          <xsl:otherwise>
	            <xsl:value-of select="-1"/>
	          </xsl:otherwise>
	        </xsl:choose>
	      </xsl:variable>   
	      <xsl:call-template name="replace-text-list">
	        <xsl:with-param name="text" select="$text"/>
	        <xsl:with-param name="replace-list" select="$replace-list"/>
	        <xsl:with-param name="replace-list-rest" select="$rest"/>
	        <xsl:with-param name="sep" select="$sep"/>
	        <xsl:choose>
	          <xsl:when test="$len!='-1' and ($len-best='-1' or $len &lt; $len-best)">
	            <xsl:with-param name="replace-best" select="$replace"/>
	            <xsl:with-param name="by-best" select="$by"/>
	            <xsl:with-param name="len-best" select="$len"/>
	          </xsl:when>
	          <xsl:otherwise>
	            <xsl:with-param name="replace-best" select="$replace-best"/>
	            <xsl:with-param name="by-best" select="$by-best"/>
	            <xsl:with-param name="len-best" select="$len-best"/>
	          </xsl:otherwise>
	        </xsl:choose>
	      </xsl:call-template>                
      </xsl:when>
      <xsl:otherwise>
	    <xsl:choose>
	      <xsl:when test="$len-best!='-1'">
	        <xsl:value-of select="substring-before($text,$replace-best)"/>
	        <xsl:value-of select="$by" disable-output-escaping="yes"/>
	        <xsl:call-template name="replace-text-list">
	          <xsl:with-param name="text" select="substring-after($text,$replace)"/>
	          <xsl:with-param name="replace-list" select="$replace-list" />
	          <xsl:with-param name="sep" select="$sep" />
	        </xsl:call-template>
	      </xsl:when>
	      <xsl:otherwise>
	        <xsl:value-of select="$text"/>
	      </xsl:otherwise>
	    </xsl:choose>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  
  <xsl:template name="string-repeat">
    <xsl:param name="str"/>
    <xsl:param name="num"/>
    
    <xsl:if test="$num>0">
      <xsl:value-of select = "$str"/>  
      <xsl:call-template name="string-repeat">
	      <xsl:with-param name="str" select="$str"/>
	      <xsl:with-param name="num" select="-1+$num"/>
      </xsl:call-template>
    </xsl:if>
    </xsl:template>

<!-- ################################################################################ -->
<!-- #    normal matching -->
<!-- ################################################################################ -->
  
  
  <xsl:template match="*|/">
    <xsl:apply-templates/>
  </xsl:template>

  <xsl:template match="text()">
    <xsl:value-of select="."/>
  </xsl:template>

  <xsl:template match="section">
    ==<xsl:value-of select="title"/>==
    <xsl:text>&n;</xsl:text>
    <xsl:apply-templates/>
  </xsl:template>

  <xsl:template match="title">
    <!-- nothing -->
  </xsl:template>

  <xsl:template match="para">
    <xsl:apply-templates/>
    <xsl:text>&n;&n;</xsl:text>
  </xsl:template>
  
  <xsl:template match="itemizedlist">
    <xsl:apply-templates/>
  </xsl:template>
  
  <xsl:template match="listitem">
    <xsl:call-template name="string-repeat">
      <xsl:with-param name="str" select="'*'"/>
      <xsl:with-param name="num" select="count(ancestor::blockquote)"/>
    </xsl:call-template>
    <xsl:text>* </xsl:text>
    <xsl:apply-templates/>
    <xsl:text>&n;</xsl:text>
  </xsl:template>

  <xsl:template match="table">
    <xsl:text>{|</xsl:text>
    <xsl:apply-templates/>
    <xsl:text>|}&n;</xsl:text>
  </xsl:template>

  <xsl:template match="row">
    <xsl:apply-templates/>
    <xsl:text>&n;|-</xsl:text>
  </xsl:template>
  
  <xsl:template match="entry">
    <xsl:apply-templates/>
    <xsl:if test="count(following-sibling::entry)>0">
      <xsl:text>||</xsl:text>
    </xsl:if>
  </xsl:template>

  <xsl:template match="ulink">
    <xsl:choose>
      <xsl:when test="@linktype='local' or @linktype='file' or @linktype='localfile' or
	                    @linktype='indexterm'">
	      <xsl:text>[[</xsl:text>
	      <xsl:choose>
	        <xsl:when test="@linktype='file'">File:</xsl:when>
	        <xsl:when test="@linktype='localfile'">Localfile:</xsl:when>
	        <xsl:when test="@linktype='indexterm'">Indexterm:</xsl:when>
	      </xsl:choose>
	      <xsl:call-template name="replace-text-list">
	        <xsl:with-param name="text" select="@url"/>
	        <xsl:with-param name="replace-list" select="':,&#58;,|,&#124;,],&#93;'"/>
	      </xsl:call-template>
	      <xsl:if test="text()!='' and text()!=@url">
	        <xsl:text>|</xsl:text>
	        <xsl:call-template name="replace-text-list">
	          <xsl:with-param name="text" select="text()"/>
	          <xsl:with-param name="replace-list" select="'|,&#124;,],&#93;'"/>
	        </xsl:call-template>
	      </xsl:if>
	      <xsl:text>]]</xsl:text>
      </xsl:when>
      <xsl:when test="@linktype ='url'">
	      <xsl:text>[</xsl:text>
	      <xsl:call-template name="replace-text-list">
	        <xsl:with-param name="text" select="@url"/>
	        <xsl:with-param name="replace-list" select="' ,&#32;,],&#93;'"/>
	      </xsl:call-template>
	      <xsl:text>]</xsl:text>
      </xsl:when>
    </xsl:choose>
  </xsl:template>

  <xsl:template match="mediaobject">
    <xsl:variable name="fileref" select="imageobject/imagedata/@fileref"/>
    <xsl:if test="$fileref !=''">
      <xsl:variable name="fileref1">
	      <xsl:choose>
	        <xsl:when test="contains($fileref,'local://')">
	          <xsl:value-of select="substring-after($fileref,'local://')"/>
	        </xsl:when>
	        <xsl:otherwise>
	          <xsl:value-of select="$fileref"/>
	        </xsl:otherwise>
	      </xsl:choose>
      </xsl:variable>
      <xsl:choose>
	      <xsl:when test="contains($fileref,'local://')">
	        <xsl:text>[[Image:</xsl:text>
	      </xsl:when>
	      <xsl:otherwise>
	        <xsl:text>[[ImageFile:</xsl:text>
	      </xsl:otherwise>
      </xsl:choose>
      <xsl:call-template name="replace-text-list">
	      <xsl:with-param name="text" select="$fileref1"/>
	      <xsl:with-param name="replace-list" select="':,&#58;,|,&#124;,],&#93;'"/>
      </xsl:call-template>
      <xsl:if test="imageobject/imagedata/@width">
        <xsl:text>|</xsl:text>
        <xsl:value-of select="imageobject/imagedata/@width"/>
      </xsl:if>
      <xsl:if test="imageobject/imagedata/@align">
        <xsl:text>|</xsl:text>
        <xsl:choose>
          <xsl:when test="imageobject/imagedata/@align = 'center'">
            <xsl:text>middle</xsl:text>
          </xsl:when>
          <xsl:otherwise>
            <xsl:value-of select="imageobject/imagedata/@align"/>
          </xsl:otherwise>
        </xsl:choose>
        <xsl:text>|none</xsl:text>
      </xsl:if>
      <xsl:if test="caption !=''">
	      <xsl:text>|</xsl:text>
	      <xsl:call-template name="replace-text-list">
	        <xsl:with-param name="text" select="caption"/>
	        <xsl:with-param name="replace-list" select="'|,&#124;,],&#93;'"/>
	      </xsl:call-template>
      </xsl:if>
      <xsl:text>]]</xsl:text>
    </xsl:if>
  </xsl:template>
  
  <xsl:template match="tcl">
    <xsl:text>&lt;tcl type="</xsl:text>
    <xsl:value-of select = "@type"/><xsl:text>"</xsl:text>
    <xsl:if test="@name != ''">
      <xsl:text> name="</xsl:text><xsl:value-of select = "@name"/><xsl:text>"</xsl:text>
    </xsl:if>
    <xsl:text>&gt;</xsl:text>
    <xsl:value-of select = "."/>
    <xsl:text>&lt;/tcl&gt;</xsl:text>
  </xsl:template>

  <xsl:template match="emphasis">
    <xsl:choose>
      <xsl:when test="@role='strong'">
	      <xsl:text>'''</xsl:text><xsl:apply-templates/><xsl:text>'''</xsl:text>
      </xsl:when>
      <xsl:when test="@role='header'">
	      ==<xsl:apply-templates/>==
      </xsl:when>
      <xsl:when test="@role='formula'">
	      <formula><xsl:apply-templates/></formula>
      </xsl:when>
      <xsl:otherwise>
	      <xsl:text>''</xsl:text><xsl:apply-templates/><xsl:text>''</xsl:text>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
    
</xsl:stylesheet>













