<?xml version="1.0" encoding="utf-8"?><!-- -*- coding: utf-8;-*- -->
<!DOCTYPE xsl:stylesheet [
<!ENTITY tab "<xsl:text>    </xsl:text>"> <!-- Tab -->
<!ENTITY n "<xsl:text>---NEWLINE---</xsl:text>"> <!-- New Line -->
]>

<xsl:stylesheet version="1.0"
xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
<xsl:output method = "text" indent = "no" />

  <xsl:param name='base_path' select='/'/>

  <xsl:template match="*|/">
    <xsl:apply-templates/>
  </xsl:template>

  <xsl:template match="text()">
    <xsl:value-of select="."/>
  </xsl:template>

  <xsl:template match="section">
    <xsl:value-of select="title"/>
    <xsl:text>&n;</xsl:text>
    <xsl:apply-templates/>
  </xsl:template>

  <xsl:template match="title">
    <!-- nothing -->
  </xsl:template>

<xsl:template match="para">
    <xsl:apply-templates/>
    <xsl:text>&n;</xsl:text>
</xsl:template>

<xsl:template match="itemizedlist">
    <xsl:apply-templates/>
</xsl:template>
<xsl:template match="listitem">
    <xsl:text>* </xsl:text>
    <xsl:apply-templates/>
    <xsl:text>&n;</xsl:text>
</xsl:template>

<xsl:template match="row">
    <xsl:apply-templates/>
    <xsl:text>&n;</xsl:text>
</xsl:template>

<xsl:template match="entry">
    <xsl:apply-templates/>
    <xsl:text>&tab;</xsl:text>
</xsl:template>

  <xsl:template match="ulink">
    <xsl:choose>
      <xsl:when test = "text() = ''">
	<xsl:value-of select="@url"/>
      </xsl:when>
    </xsl:choose>
    <xsl:apply-templates/>
  </xsl:template>

  <xsl:template match="mediaobject">
    <!-- nothing -->
  </xsl:template>

  <xsl:template match="tcl">
    <xsl:choose>
      <xsl:when test = "@name = ''">
	<xsl:text>TCL</xsl:text>
      </xsl:when>
      <xsl:otherwise>
	<xsl:value-of select="@name"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

</xsl:stylesheet>













