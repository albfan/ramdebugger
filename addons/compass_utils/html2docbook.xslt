<?xml version="1.0" encoding="utf-8"?><!-- -*- coding: utf-8;-*- -->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
  <xsl:output method="xml" indent="yes"/>

<!-- ******************************************************************************** -->
<!-- lowercase func -->
<!-- ******************************************************************************** -->

  <xsl:template name="att">
    <xsl:for-each select="@*">
      <xsl:attribute name="{translate(name(),'ABCDEFGHIJKLMNOPQRSTUVWXYZ',
	'abcdefghijklmnopqrstuvwxyz')}">
	<xsl:value-of select="."/>
      </xsl:attribute>
    </xsl:for-each>
    <xsl:apply-templates/>
  </xsl:template>

<!-- ******************************************************************************** -->
<!-- main match -->
<!-- ******************************************************************************** -->

  <xsl:template match="*">
    <xsl:apply-templates select="*|text()"/>
  </xsl:template>

  <xsl:template match="html">
    <xsl:apply-templates select="body"/>
  </xsl:template>

  <xsl:template match="body">
    <wordwidget>
      <xsl:apply-templates/>
    </wordwidget>
  </xsl:template>

<!-- ******************************************************************************** -->
<!-- basic formatting -->
<!-- ******************************************************************************** -->

  <xsl:template match="br">
    <!-- tag br is modified later in TCL code -->
    <br><xsl:apply-templates select="*"/></br>
  </xsl:template>

  <xsl:template match="h1|h2|h3|h4">
    <emphasis role="header">
      <xsl:apply-templates/>
    </emphasis>
  </xsl:template>

  <xsl:template match="p|div">
    <para>
      <xsl:apply-templates/>
    </para>
  </xsl:template>

  <xsl:template match="div[.//div]">
    <blockquote>
      <xsl:apply-templates/>
    </blockquote>
  </xsl:template>

  <xsl:template match="u">
    <xsl:apply-templates/>
  </xsl:template>

  <xsl:template match="span">
    <xsl:apply-templates/>
  </xsl:template>


  <xsl:template match="font">
    <xsl:apply-templates/>
  </xsl:template>

  <xsl:template match="blockquote">
    <blockquote>
      <xsl:apply-templates/>
    </blockquote>
  </xsl:template>

  <xsl:template match="b|strong">
    <emphasis role="strong">
      <xsl:apply-templates/>
    </emphasis>
  </xsl:template>
  <xsl:template match="t|em|i">
    <emphasis role="">
      <xsl:apply-templates/>
    </emphasis>
  </xsl:template>
  <xsl:template match="a[//img]">
    <xsl:apply-templates/>
  </xsl:template>

  <xsl:template match="a[@href]">
    <xsl:choose>
      <xsl:when test=".//img">
	<xsl:apply-templates/>
      </xsl:when>
      <xsl:otherwise>
	<ulink linktype="url">
	  <xsl:attribute name="url">
	    <xsl:value-of select="@href"/>
	  </xsl:attribute>
	  <xsl:apply-templates/>
	</ulink>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  
  <xsl:template match="a">
    <xsl:apply-templates/>
  </xsl:template>

  <xsl:template match="sub">
    <subscript><xsl:apply-templates/></subscript>
  </xsl:template>
  
  <xsl:template match="sup">
    <superscript><xsl:apply-templates/></superscript>
  </xsl:template>
 
<!-- ******************************************************************************** -->
<!-- tables -->
<!-- ******************************************************************************** -->

  <xsl:template match="table">
    <table>
      <xsl:apply-templates/>
    </table>
  </xsl:template>
  <xsl:template match="tr">
    <row>
      <xsl:apply-templates/>
    </row>
  </xsl:template>
  <xsl:template match="td">
    <entry>
      <xsl:apply-templates/>
    </entry>
  </xsl:template>

<!-- ******************************************************************************** -->
<!-- lists -->
<!-- ******************************************************************************** -->

  <xsl:template match="ul|menu">
    <itemizedlist>
      <xsl:apply-templates/>
    </itemizedlist>
  </xsl:template>
  <xsl:template match="li">
    <xsl:choose>
      <xsl:when test="parent::ul|parent::menu">
	<listitem>
	  <xsl:apply-templates/>
	</listitem>
      </xsl:when>
      <xsl:otherwise>
	<itemizedlist>
	  <listitem>
	    <xsl:apply-templates/>
	  </listitem>
	</itemizedlist>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

<!-- ******************************************************************************** -->
<!-- images -->
<!-- ******************************************************************************** -->

  <xsl:template match="img">
    <mediaobject>
      <imageobject>
	<imagedata>
	  <xsl:attribute name="width">
	    <xsl:value-of select="@width"/>
	  </xsl:attribute>
	  <xsl:apply-templates/>
	  <xsl:attribute name="fileref">
	    <xsl:value-of select="@src"/>
	  </xsl:attribute>
	  <xsl:apply-templates/>
	</imagedata>
      </imageobject>
    </mediaobject>
  </xsl:template>

<!-- ******************************************************************************** -->
<!-- selection fragments -->
<!-- ******************************************************************************** -->

  <xsl:template match="startfragment">
    <startselection/>
  </xsl:template>
  <xsl:template match="endfragment">
    <endselection/>
  </xsl:template>


</xsl:stylesheet>
