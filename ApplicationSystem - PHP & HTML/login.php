<!DOCTYPE html>
<html>
<head>
	<title>Login</title>
</head>
<body>
<?php
	session_start();

	if(isset($_POST['return'])){
		session_destroy();
		header("Location: main.html");
	}

	if(isset($_POST['submit'])){
		$_SESSION['email'] = $_POST['email'];
		$_SESSION['password'] = sha1($_POST['password']);
		if($_SESSION['instruction'] === "review"){
			header("Location: review.php");
		} else if($_SESSION['instruction'] === "update"){
			header("Location: updateApp.php");
		}
	}

	echo <<<BODY
	<form action="login.php" method="post">
		<b>Email associated with application:</b><input type="email" name="email"><br><br>
		<b>Password associated with application:</b><input type="password" name="password">
BODY;
	
	if(isset($_SESSION['loginMessage'])){
		echo "<br>";
		echo $_SESSION['loginMessage'];
		unset($_SESSION['loginMessage']);
	}

	echo <<<BODY
		<br><br><input type="submit" name="submit" value="Submit"><br><br>
		<input type="submit" name="return" value="Return to main menu">

	</form>
BODY;


?>	
</body>
</html>