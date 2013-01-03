<?xml version="1.0" encoding="utf-8"?><!-- -*- coding: utf-8;-*- -->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0"
  xmlns:w="http://schemas.microsoft.com/office/word/2003/wordml" xmlns:v="urn:schemas-microsoft-com:vml" xmlns:w10="urn:schemas-microsoft-com:office:word" xmlns:sl="http://schemas.microsoft.com/schemaLibrary/2003/core" xmlns:aml="http://schemas.microsoft.com/aml/2001/core" xmlns:wx="http://schemas.microsoft.com/office/word/2003/auxHint" xmlns:o="urn:schemas-microsoft-com:office:office" xmlns:dt="uuid:C2F41010-65B3-11d1-A29F-00AA00C14882" w:macrosPresent="no" w:embeddedObjPresent="no" w:ocxPresent="no" xml:space="preserve">
  <xsl:output method="xml" indent="yes"/>

  <xsl:param name="default_image_width" select="385"/>
  <xsl:param name="page_internal_width" select="425"/>
<!-- ******************************************************************************** -->
<!-- error checking -->
<!-- ******************************************************************************** -->
  
  <xsl:template match="*">
    <xsl:param name='ispara' select='0'/>
    <xsl:choose>
      <xsl:when test="self::title|self::emphasis[@role='header']">
	<!-- nothing -->
      </xsl:when>
      <xsl:otherwise>
	<xsl:choose>
	  <xsl:when test = "$ispara = 0">
	    <w:p>
	      <w:r>
		<w:rPr>
		  <w:color w:val="FF0000"/>
		</w:rPr>
		<w:t><xsl:value-of select="concat('&lt;',name(),'&gt;')"/></w:t>
	      </w:r>
	    </w:p>
	  </xsl:when> 
	  <xsl:otherwise>
	    <w:r>
	      <w:rPr>
		<w:color w:val="FF0000"/>
	      </w:rPr>
	      <w:t><xsl:value-of select="concat('&lt;',name(),'&gt;')"/></w:t>
	    </w:r>
	  </xsl:otherwise>
	</xsl:choose>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  
<!-- ******************************************************************************** -->
<!-- main match -->
  <!-- ******************************************************************************** -->
  
  <xsl:template match="text()">
    <xsl:param name="b_style"/>
    <xsl:param name="i_style"/>
    <xsl:param name="color_style"/>
    <xsl:param name="sub_style"/>
    <xsl:param name="sup_style"/>
    <xsl:param name="literal_style"/>
    <xsl:call-template name="print-text">
      <xsl:with-param name="text" select='.'/>
      <xsl:with-param name="b_style" select="$b_style"/>
      <xsl:with-param name="i_style" select="$i_style"/>
      <xsl:with-param name="color_style" select="$color_style"/>
      <xsl:with-param name="sub_style" select="$sub_style"/>
      <xsl:with-param name="sup_style" select="$sup_style"/>
      <xsl:with-param name="literal_style" select="$literal_style"/>
    </xsl:call-template>
  </xsl:template>

  <xsl:template name='print-text'>
    <xsl:param name="b_style"/>
    <xsl:param name="i_style"/>
    <xsl:param name="color_style"/>
    <xsl:param name="sub_style"/>
    <xsl:param name="sup_style"/>
    <xsl:param name="literal_style"/>
    <xsl:param name="text"/>
    <w:r>
      <w:rPr>
	<xsl:if test="$b_style &gt; 0">
	  <w:b/>
	</xsl:if>
	<xsl:if test="$i_style &gt; 0">
	  <w:i/>
	</xsl:if>
	<xsl:if test="$color_style != ''">
	  <w:color w:val="{$color_style}"/>
	</xsl:if>
	<xsl:if test="$sub_style != ''">
	  <w:vertAlign w:val="subscript"/>
	</xsl:if>
	<xsl:if test="$sup_style != ''">
	  <w:vertAlign w:val="superscript"/>
	</xsl:if>
	<xsl:if test="$literal_style != ''">
	  <w:rStyle w:val="literal"/>
	</xsl:if>
      </w:rPr>
      <w:t><xsl:value-of select="$text"/></w:t>
    </w:r>
  </xsl:template>

  <xsl:template match="wordnoter|lognoter">
    <?mso-application progid="Word.Document"?>
    <w:wordDocument xmlns:w="http://schemas.microsoft.com/office/word/2003/wordml" xmlns:v="urn:schemas-microsoft-com:vml" xmlns:w10="urn:schemas-microsoft-com:office:word" xmlns:sl="http://schemas.microsoft.com/schemaLibrary/2003/core" xmlns:aml="http://schemas.microsoft.com/aml/2001/core" xmlns:wx="http://schemas.microsoft.com/office/word/2003/auxHint" xmlns:o="urn:schemas-microsoft-com:office:office" xmlns:dt="uuid:C2F41010-65B3-11d1-A29F-00AA00C14882" w:macrosPresent="no" w:embeddedObjPresent="no" w:ocxPresent="no" xml:space="preserve">
      <w:docPr>
	<w:view w:val="print"/>
	<w:zoom w:percent="75"/>
      </w:docPr>
      <w:styles>
	<w:style w:type="paragraph" w:styleId="heading 1">
	  <w:name w:val="heading 1"/>
	  <w:basedOn w:val="Normal"/>
	  <w:next w:val="Normal"/>
	  <w:rPr>
	    <wx:font wx:val="Arial"/>
	    <w:b/>
	    <w:sz w:val="32"/>
	  </w:rPr>
	</w:style>
	<w:style w:type="paragraph" w:styleId="heading 2">
	  <w:name w:val="heading 2"/>
	  <w:basedOn w:val="Normal"/>
	  <w:next w:val="Normal"/>
	  <w:rPr>
	    <wx:font wx:val="Arial"/>
	    <w:b/>
	    <w:i/>
	    <w:sz w:val="28"/>
	  </w:rPr>
	</w:style>
	<w:style w:type="paragraph" w:styleId="heading 3">
	  <w:name w:val="heading 3"/>
	  <w:basedOn w:val="Normal"/>
	  <w:next w:val="Normal"/>
	  <w:rPr>
	    <wx:font wx:val="Arial"/>
	    <w:b/>
	    <w:i/>
	    <w:sz w:val="28"/>
	  </w:rPr>
	</w:style>
	<w:style w:type="paragraph" w:styleId="heading 4">
	  <w:name w:val="heading 4"/>
	  <w:basedOn w:val="Normal"/>
	  <w:next w:val="Normal"/>
	  <w:rPr>
	    <wx:font wx:val="Arial"/>
	    <w:b/>
	    <w:i/>
	    <w:sz w:val="28"/>
	  </w:rPr>
	</w:style>
	<w:style w:type="paragraph" w:default="on" w:styleId="Normal">
	  <w:name w:val="Normal"/>
	  <w:pPr>
	    <w:jc w:val="both"/>
	  </w:pPr>
	  <w:rPr>
	    <wx:font wx:val="Times New Roman"/>
	    <w:sz w:val="24"/>
	  </w:rPr>
	</w:style>
	<w:style w:type="character" w:styleId="literal">
	  <w:name w:val="literal"/>
	  <w:basedOn w:val="Normal"/>
	  <w:rPr>
	      <w:rFonts w:ascii="Courier New" w:h-ansi="Courier New" w:cs="Courier New" w:hint="default"/>
	  </w:rPr>
	</w:style>
      </w:styles>
      <w:lists>
	<w:listDef w:listDefId="0">
	  <w:lvl w:ilvl="0">
	    <w:start w:val="1"/>
	    <w:nfc w:val="23"/>
	    <w:lvlText w:val="·"/>
	    <w:lvlJc w:val="left"/>
	    <w:pPr>
	      <w:tabs>
		<w:tab w:val="list" w:pos="720"/>
	      </w:tabs>
	      <w:ind w:left="720" w:hanging="360"/>
	    </w:pPr>
	    <w:rPr>
	      <w:rFonts w:ascii="Symbol" w:h-ansi="Symbol" w:hint="default"/>
	    </w:rPr>
	  </w:lvl>
	  <w:lvl w:ilvl="1" w:tentative="on">
	    <w:start w:val="1"/>
	    <w:nfc w:val="23"/>
	    <w:lvlText w:val="o"/>
	    <w:lvlJc w:val="left"/>
	    <w:pPr>
	      <w:tabs>
		<w:tab w:val="list" w:pos="1440"/>
	      </w:tabs>
	      <w:ind w:left="1440" w:hanging="360"/>
	    </w:pPr>
	    <w:rPr>
	      <w:rFonts w:ascii="Courier New" w:h-ansi="Courier New" w:cs="Courier New" w:hint="default"/>
	    </w:rPr>
	  </w:lvl>
	  <w:lvl w:ilvl="2" w:tentative="on">
	    <w:start w:val="1"/>
	    <w:nfc w:val="23"/>
	    <w:lvlText w:val="Â§"/>
	    <w:lvlJc w:val="left"/>
	    <w:pPr>
	      <w:tabs>
		<w:tab w:val="list" w:pos="2160"/>
	      </w:tabs>
	      <w:ind w:left="2160" w:hanging="360"/>
	    </w:pPr>
	    <w:rPr>
	      <w:rFonts w:ascii="Wingdings" w:h-ansi="Wingdings" w:hint="default"/>
	      
	    </w:rPr>
	  </w:lvl>
	</w:listDef>
	<!-- Definition for ordered list -->
	<w:listDef w:listDefId="1">
	  <w:lvl w:ilvl="0">
	    <w:start w:val="1"/>
	    <w:lvlText w:val="%1."/>
	    <w:lvlJc w:val="left"/>
	    <w:pPr>
	      <w:tabs>
		<w:tab w:val="list" w:pos="720"/>
	      </w:tabs>
	      <w:ind w:left="720" w:hanging="360"/>
	    </w:pPr>
	  </w:lvl>
	  <w:lvl w:ilvl="1" w:tentative="on">
	    <w:start w:val="1"/>
	    <w:nfc w:val="4"/>
	    <w:lvlText w:val="%2."/>
	    <w:lvlJc w:val="left"/>
	    <w:pPr>
	      <w:tabs>
		<w:tab w:val="list" w:pos="1440"/>
	      </w:tabs>
	      <w:ind w:left="1440" w:hanging="360"/>
	    </w:pPr>
	  </w:lvl>
	  <w:lvl w:ilvl="2" w:tentative="on">
	    <w:start w:val="1"/>
	    <w:nfc w:val="2"/>
	    <w:lvlText w:val="%3."/>
	    <w:lvlJc w:val="right"/>
	    <w:pPr>
	      <w:tabs>
		<w:tab w:val="list" w:pos="2160"/>
	      </w:tabs>
	      <w:ind w:left="2160" w:hanging="180"/>
	    </w:pPr>
	  </w:lvl>
	</w:listDef>
	<!-- List instances -->
	<!-- For each unordered list create list instnace -->
	<xsl:for-each select="/descendant::itemizedlist">
	  <w:list w:ilfo="{position()}">
	    <!-- Bound to definition #0 (see above) -->
	    <w:ilst w:val="0"/>
	  </w:list>
	</xsl:for-each>
	<!-- For each ordered list create list instance -->
	<xsl:for-each select="/descendant::orderedlist[@continuation='restarts']">
	  <w:list w:ilfo="{count(/descendant::itemizedlist) + position()}">
	    <!-- Bound to definition #1 (see above) -->
	    <w:ilst w:val="1"/>
	    <w:lvlOverride>
	      <w:startOverride w:val="1"/>
	    </w:lvlOverride>
	  </w:list>
	</xsl:for-each>
      </w:lists>
      <w:body>
	<wx:sect>
	  <xsl:apply-templates/>
	  <w:sectPr>
	    <w:pgSz w:w="11906" w:h="16838"/>
	    <w:pgMar w:top="1417" w:right="1701" w:bottom="1417" w:left="1701" w:header="708" w:footer="708" w:gutter="0"/>
	  </w:sectPr>
	</wx:sect>
      </w:body>
    </w:wordDocument>
  </xsl:template>
  
  <!-- ******************************************************************************** -->
  <!-- section -->
  <!-- ******************************************************************************** -->
  
  <xsl:template match="section">
    <xsl:variable name="seclevel" select="count(ancestor-or-self::section)"/>
    <wx:sub-section>
      <w:p>
	<w:pPr><w:pStyle w:val="{concat('heading ',$seclevel)}"/></w:pPr>
	<w:r>
	  <w:t>
	    <xsl:call-template name="substring-after-last">
	      <xsl:with-param name="string" select='title'/>
	      <xsl:with-param name='after-string' select='"&gt;"'/>
	    </xsl:call-template>
	  </w:t>
	</w:r>
      </w:p>
      <xsl:apply-templates/>
    </wx:sub-section>
  </xsl:template>
  
  <xsl:template name='substring-after-last'>
    <xsl:param name='string' select=''/>
    <xsl:param name='after-string' select=''/>
    <xsl:choose>
      <xsl:when test="substring-after($string,$after-string)=$string">
	<xsl:value-of select="$string"/>
      </xsl:when>
      <xsl:otherwise>
	<xsl:call-template name="substring-after-last">
	  <xsl:with-param name="string" select='substring-after($string,$after-string)'/>
	  <xsl:with-param name='after-string' select='after-string'/>
	</xsl:call-template>
      </xsl:otherwise>
    </xsl:choose>
    </xsl:template>
  
  <!-- ******************************************************************************** -->
  <!-- para -->
  <!-- ******************************************************************************** -->
  
  <xsl:template name='para-properties'>
    <xsl:param name='blockquote_style' select='0'/>
    <w:pPr>
      <xsl:if test="$blockquote_style &gt; 0">
	<w:ind w:left="{708*$blockquote_style}"/>
      </xsl:if>
      <xsl:if test="descendant::emphasis[@role='header']">
	<xsl:variable name="seclevel" select="count(ancestor-or-self::name)+1"/>
	<w:pStyle w:val="{concat('heading ',$seclevel)}"/>
      </xsl:if>
      <xsl:if test="descendant::imagedata[@align='center' or @align='']">
	<w:jc w:val="center"/>
      </xsl:if>
    </w:pPr>
  </xsl:template>

  <xsl:template match="para">
    <xsl:param name='blockquote_style' select='0'/>
    <w:p>
      <xsl:call-template name="para-properties">
	<xsl:with-param name="blockquote_style" select='$blockquote_style'/>
      </xsl:call-template>
      <xsl:apply-templates>
	<xsl:with-param name="ispara" select='1'/>
      </xsl:apply-templates>
    </w:p>
  </xsl:template>

  <xsl:template match="para[mediaobject/caption != '']">
    <xsl:param name='blockquote_style' select='0'/>
    <xsl:if test="preceding-sibling::mediaobject">
      <w:p>
	<xsl:call-template name="para-properties">
	  <xsl:with-param name="blockquote_style" select='$blockquote_style'/>
	</xsl:call-template>
	<xsl:apply-templates select="preceding-sibling::mediaobject">
	  <xsl:with-param name="ispara" select='1'/>
	</xsl:apply-templates>
      </w:p>
    </xsl:if>

    <w:p>
      <xsl:call-template name="para-properties">
	<xsl:with-param name="blockquote_style" select='$blockquote_style'/>
      </xsl:call-template>
      <xsl:apply-templates select="mediaobject">
	<xsl:with-param name="ispara" select='1'/>
      </xsl:apply-templates>
    </w:p>
    <w:p>
      <w:pPr>
	<w:jc w:val="center"/>
      </w:pPr>
      <w:r>
	<w:t><xsl:value-of select="mediaobject/caption"/></w:t>
      </w:r>
    </w:p>
    <w:p/>

    <xsl:if test="following-sibling::mediaobject">
      <w:p>
	<xsl:call-template name="para-properties">
	  <xsl:with-param name="blockquote_style" select='$blockquote_style'/>
	</xsl:call-template>
	<xsl:apply-templates select="following-sibling::mediaobject">
	  <xsl:with-param name="ispara" select='1'/>
	</xsl:apply-templates>
      </w:p>
    </xsl:if>
  </xsl:template>

  <xsl:template match="para[descendant::table]">
    <xsl:if test="preceding-sibling::table">
      <w:p>
	<xsl:call-template name="para-properties">
	  <xsl:with-param name="blockquote_style" select='$blockquote_style'/>
	</xsl:call-template>
	<xsl:apply-templates select="preceding-sibling::table">
	  <xsl:with-param name="ispara" select='1'/>
	</xsl:apply-templates>
      </w:p>
    </xsl:if>

    <xsl:apply-templates select=".//table"/>

    <xsl:if test="following-sibling::table">
      <w:p>
	<xsl:call-template name="para-properties">
	  <xsl:with-param name="blockquote_style" select='$blockquote_style'/>
	</xsl:call-template>
	<xsl:apply-templates select="following-sibling::table">
	  <xsl:with-param name="ispara" select='1'/>
	</xsl:apply-templates>
      </w:p>
    </xsl:if>
  </xsl:template>

  
  <!-- ******************************************************************************** -->
  <!-- styles -->
  <!-- ******************************************************************************** -->
  
  <xsl:template match="emphasis[@role='strong']">
    <xsl:param name='b_style' select='0'/>
    <xsl:apply-templates>
      <xsl:with-param name="b_style" select='$b_style+1'/>
    </xsl:apply-templates>
  </xsl:template>

  <xsl:template match="emphasis[@role='italic' or @role='' or not(@role)]|literal">
    <xsl:param name='i_style' select='0'/>
    <xsl:apply-templates>
      <xsl:with-param name="i_style" select='$i_style+1'/>
    </xsl:apply-templates>
  </xsl:template>

  <xsl:template match="emphasis[@role='formula']">
    <xsl:apply-templates/>
  </xsl:template>

  <xsl:template match="emphasis[@role='warning']">
    <xsl:apply-templates>
      <xsl:with-param name="color_style" select="'FF0000'"/>
    </xsl:apply-templates>
  </xsl:template>

  <xsl:template match="emphasis[@role='header']">
    <xsl:apply-templates/>
  </xsl:template>

  <xsl:template match="blockquote">
    <xsl:param name='blockquote_style' select='0'/>
    <xsl:apply-templates>
      <xsl:with-param name="blockquote_style" select='$blockquote_style+1'/>
    </xsl:apply-templates>
  </xsl:template>

  <xsl:template match="sub|subscript">
    <xsl:apply-templates>
      <xsl:with-param name="sub_style" select='1'/>
    </xsl:apply-templates>
  </xsl:template>

  <xsl:template match="sup|superscript">
    <xsl:apply-templates>
      <xsl:with-param name="sup_style" select='1'/>
    </xsl:apply-templates>
  </xsl:template>

  <xsl:template match="literal">
    <xsl:apply-templates>
      <xsl:with-param name="literal_style" select='1'/>
    </xsl:apply-templates>
  </xsl:template>

  <!-- ******************************************************************************** -->
  <!-- lists -->
  <!-- ******************************************************************************** -->
  
  <xsl:template name='listitem-properties'>
    <w:pPr>
      <w:listPr>
	<!-- List level -->
	<w:ilvl w:val="{count(ancestor::itemizedlist|ancestor::orderedlist|ancestor::blockquote)-1}"/>
	<!-- List binding -->
	<w:ilfo>
	  <xsl:attribute name="w:val">
	    <xsl:choose>
	      <xsl:when test="parent::itemizedlist">
		<xsl:value-of select="count(preceding::itemizedlist) + 1"/>
	      </xsl:when>
	      <xsl:otherwise>
		<xsl:value-of select="count(/descendant::itemizedlist) + count(preceding::orderedlist[@continuation='restarts']|ancestor::orderedlist[@continuation='restarts'])"/>
	      </xsl:otherwise>
	    </xsl:choose>
	  </xsl:attribute>
	</w:ilfo>
      </w:listPr>
    </w:pPr>
  </xsl:template>
  
  <xsl:template match="itemizedlist|orderedlist">
    <xsl:if test="ancestor::listitem">
      <xsl:text disable-output-escaping="yes"><![CDATA[</w:p>]]></xsl:text>
    </xsl:if>
    <xsl:apply-templates/>
    <xsl:if test="ancestor::listitem">
      <xsl:text disable-output-escaping="yes"><![CDATA[<w:p>]]></xsl:text>
      <xsl:call-template name="listitem-properties"/>
    </xsl:if>
  </xsl:template>
  
  <xsl:template match="listitem">
    <w:p>
      <xsl:call-template name="listitem-properties"/>
      <xsl:apply-templates/>
    </w:p>
  </xsl:template>

  <!-- ******************************************************************************** -->
  <!-- images -->
  <!-- ******************************************************************************** -->
  
  <xsl:template match="mediaobject">
    <xsl:apply-templates/>
  </xsl:template>
  <xsl:template match="imageobject">
    <xsl:apply-templates select="imagedata"/>
  </xsl:template>
  
  <xsl:template match="imagedata">
    <xsl:variable name='default_image_widthL'>
      <xsl:choose>
	<xsl:when test="../../../ancestor::row">
	  <xsl:value-of select="$page_internal_width div count(../../../ancestor::row/entry)"/>
	</xsl:when>
	<xsl:otherwise>
	  <xsl:value-of select="$default_image_width"/>
	</xsl:otherwise>
      </xsl:choose>
    </xsl:variable>

    <xsl:variable name='n'>
      <xsl:choose>
	<xsl:when test="starts-with(@localname,'local://')">
	  <xsl:value-of select=
	  "concat('wordml://',substring-after(@localname,'local://'))"/>
	</xsl:when>
	<xsl:otherwise>
	  <xsl:value-of select="concat('wordml://',@localname)"/>
	</xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <w:r>
      <w:pict>
	<w:binData w:name="{$n}">
	  <xsl:value-of select="substring-after(@fileref,'base64,')"/>
	</w:binData>
	<xsl:variable name='w'>
	  <xsl:choose>
	    <xsl:when test="@width_p &gt; $default_image_widthL">
	      <xsl:value-of select="$default_image_widthL"/>
	    </xsl:when>
	    <xsl:otherwise>
	      <xsl:value-of select="@width_p"/>
	    </xsl:otherwise>
	  </xsl:choose>
	</xsl:variable>
	
	<xsl:variable name='h'>
	  <xsl:choose>
	    <xsl:when test="@width_p &gt; $default_image_widthL">
	      <xsl:value-of select="$default_image_widthL*@height_p div @width_p"/>
	    </xsl:when>
	    <xsl:otherwise>
	      <xsl:value-of select="@height_p"/>
	    </xsl:otherwise>
	  </xsl:choose>
	</xsl:variable>
	<v:shape style="{concat('width:',$default_image_width,'pt')}">
	  <xsl:attribute name="style">
	    <xsl:if test="@width_p">
	      <xsl:value-of select="concat('width:',$w,'pt;height:',$h,'pt')"/>
	    </xsl:if>
	  </xsl:attribute>
	  <v:imagedata src="{$n}" croptop="-2500f" cropbottom="-2500f"/>
	</v:shape>
      </w:pict>
    </w:r>
  </xsl:template>

  <xsl:template match="caption">
<!--     nothing -->
  </xsl:template>

  <!-- ******************************************************************************** -->
  <!-- tcl -->
  <!-- ******************************************************************************** -->

  <xsl:template match="tcl">
    <xsl:call-template name="print-text">
      <xsl:with-param name="color_style">00FF00</xsl:with-param>
      <xsl:with-param name="text" select='@name'/>
    </xsl:call-template>
  </xsl:template>

  <!-- ******************************************************************************** -->
  <!-- links -->
  <!-- ******************************************************************************** -->
  
  <xsl:template match="ulink">
    <xsl:call-template name="print-text">
      <xsl:with-param name="color_style">0000FF</xsl:with-param>
      <xsl:with-param name="text">
	<xsl:choose>
	  <xsl:when test="string(.) != ''">
	    <xsl:value-of select='.'/>
	  </xsl:when>
	  <xsl:otherwise>
	    <xsl:value-of select='@url'/>
	  </xsl:otherwise>
	</xsl:choose>
      </xsl:with-param>
    </xsl:call-template>
  </xsl:template>
  
  <xsl:template match="ulink[mediaobject]">
    <xsl:apply-templates/>
  </xsl:template>
  
  <!-- ******************************************************************************** -->
  <!-- tables match -->
  <!-- ******************************************************************************** -->
  
  <xsl:template match="table">
    <xsl:param name="ispara" select="0"/>
    
    <xsl:if test="$ispara">
      <xsl:text disable-output-escaping="yes"><![CDATA[</w:p>]]></xsl:text>
    </xsl:if>
    <w:tbl>
      <xsl:apply-templates/>
    </w:tbl>
    <xsl:if test="$ispara">
      <xsl:text disable-output-escaping="yes"><![CDATA[<w:p>]]></xsl:text>
      <xsl:call-template name="listitem-properties"/>
    </xsl:if>
  </xsl:template>
  
  <xsl:template match="tgroup">
    <xsl:apply-templates/>
  </xsl:template>
  
  <xsl:template match="tbody">
    <xsl:apply-templates/>
  </xsl:template>
  
  <xsl:template match="colspec">
    <xsl:apply-templates/>
  </xsl:template>

  <xsl:template match="row">
    <w:tr>
      <xsl:apply-templates/>
    </w:tr>
  </xsl:template>
  
  <xsl:template match="entry">
    <w:tc>
      <xsl:choose>
	<xsl:when test="para|listitem|itemizedlist">
	  <xsl:apply-templates/>
	</xsl:when>
	<xsl:otherwise>
	  <w:p>
	    <xsl:call-template name="para-properties"/>
	    <xsl:apply-templates/>
	  </w:p>
	</xsl:otherwise>
      </xsl:choose>
    </w:tc>
  </xsl:template>

  <!-- ******************************************************************************** -->
  <!-- index -->
  <!-- ******************************************************************************** -->

  <xsl:template match="indexterm">
  </xsl:template>
  
</xsl:stylesheet>










