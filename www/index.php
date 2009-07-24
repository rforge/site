
<!-- This is the project specific website template -->
<!-- It can be changed as liked or replaced by other content -->

<?php

$domain=ereg_replace('[^\.]*\.(.*)$','\1',$_SERVER['HTTP_HOST']);
$group_name=ereg_replace('([^\.]*)\..*$','\1',$_SERVER['HTTP_HOST']);
$themeroot='http://r-forge.r-project.org/themes/rforge/';

echo '<?xml version="1.0" encoding="UTF-8"?>';
?>
<!DOCTYPE html
	PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"
	"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en   ">

  <head>
	<meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
	<title><?php echo $group_name; ?></title>
	<link href="<?php echo $themeroot; ?>styles/estilo1.css" rel="stylesheet" type="text/css" />
  </head>

<body>

<! --- R-Forge Logo --- >
<table border="0" width="100%" cellspacing="0" cellpadding="0">
<tr><td>
<a href="/"><img src="<?php echo $themeroot; ?>/images/logo.png" border="0" alt="R-Forge Logo" /> </a> </td> </tr>
</table>


<!-- get project title  -->
<!-- own website starts here, the following may be changed as you like -->

<?php if ($handle=fopen('http://'.$domain.'/export/projtitl.php?group_name='.$group_name,'r')){
$contents = '';
while (!feof($handle)) {
	$contents .= fread($handle, 8192);
}
fclose($handle);
echo $contents; } ?>

<!-- end of project description -->

<h3>R-Forge Build Check Schedule</h3>

<p><b>Rebuild R</b>: 19:00 CET.</p>
<hr>
<p><b>Exportation of R package sources</b> (this also includes an update of the R Packages tab): 20:30 CET.</p>
<p><b>Sync unpacked package sources to build machine</b>: 21:45 CET.</p>
<p><b>Building of R source packages (.tar.gz)</b>: 22:00 CET.</p>
<p><b>Sync package sources (.tar.gz) to R-Forge</b>: 23:30 CET.</p>
<hr>
<p><b>Sync package sources (.tar.gz) to build machines</b>: 23:45 CET.</p>
<p><b>Building of Windows binary packages</b>: 0:00 (patched), 12:30 (devel) CET.</p>
<p><b>Building of Mac OS X (universal) binary packages</b>: 0:00 (patched) CET.</p>
<p><b>Sync package binaries (.zip, .tgz) and build logs to R-Forge</b>: 2:30 CET.</p>
<hr>
<p><b>Sync package sources to check machines (.tar.gz)</b>: 1:30 CET.</p>
<p><b>Checking of packages (Linux)</b>: 2:00 (devel), 10:00 (patched) CET.</p>
<p><b>Checking of packages (Windows)</b>: 2:00 (devel), 10:00 (patched) CET.</p>
<p><b>Checking of packages (Mac)</b>: 2:00 (patched) CET.</p>
<p><b>Sync check results to R-Forge</b>: 10:00, 18:00 CET.</p>
<hr>
<p>Build/check cycle completed.</p>

<p> For general information about this project please visit the <a href="http://<?php echo $domain; ?>/projects/<?php echo $group_name; ?>/">project summary page</a>. </p>

</body>
</html>
