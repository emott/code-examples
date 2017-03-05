<!DOCTYPE html>
<html>
<head>
	<title>Submit Application</title>
</head>
<body>
<?php
	if(isset($_POST['return'])){
		header("Location: main.html");
	}

	echo <<<BODY
	<form action="confirmation.php" method="post">
		<b>Name:</b><input type="text" name="name" required><br><br>
		<b>Email:</b><input type="email" name="email" required><br><br>
		<b>GPA:</b><input type="number" name="gpa" min="0.0" max="4.0" step=".1" required><br><br>

		<b>Year:</b><input type="radio" name="year" value="10" required>10
		<input type="radio" name="year" value="11" required>11
		<input type="radio" name="year" value="12" required>12<br><br>

		<b>Gender</b><input type="radio" name="gender" value="M" required>M
		<input type="radio" name="gender" value="F" required>F<br><br>

		<b>Password:</b><input type="password" name="password" required><br><br>
		<b>Verify Password:</b><input type="password" name="verifyPass" required><br><br>

		<input type="submit" name="submit" value="Submit Data"><br><br>
	
	</form>

	<form action="submitApp.php" method="post">
		<input type="submit" name="return" value="Return to main menu">
	</form>
BODY;
?>
</body>
</html>