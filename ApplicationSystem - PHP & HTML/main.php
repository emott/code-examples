<?php
		session_start();

		if(isset($_POST["submit"])){
			header("Location: submitApp.php");
		} 
		if(isset($_POST['review'])){
			$_SESSION['instruction'] = "review";
			header("Location: login.php");
		}
		if(isset($_POST['update'])){
			$_SESSION['instruction'] = "update";
			header("Location: login.php");
		}
		if(isset($_POST['admin'])){
			function validate($user,$pass) {
			    $users = array('main' => '26375fe3d0d9f405b0181bf8ef3b0973b8dbb133'); // main => terps as sha1
			    if (isset($users[$user]) && ($users[$user] == sha1($pass))) {
			        return true;
			    } else {
			        return false;
			    }
			}

			if (!validate($_SERVER['PHP_AUTH_USER'], $_SERVER['PHP_AUTH_PW'])) {
			    header("WWW-Authenticate: Basic realm=\"Admin Page\"");
			    header('HTTP/1.0 401 Unauthorized');
			    echo "You must login to access admin page";
			    exit;
			} else {
				header("Location: admin.php");
			}
		}

?>