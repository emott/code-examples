<!DOCTYPE html>
<html>
<head>
	<title>Confirmation</title>
</head>
<body>
<?php
	if(isset($_POST['return'])){
		header("Location: main.html");
	}

	$name = $_POST['name'];
	$email = $_POST['email'];
	$gpa = $_POST['gpa'];
	$year = $_POST['year'];
	$gender = $_POST['gender'];
	$password = sha1($_POST['password']);



	$db_connection = new mysqli("localhost", "dbuser", "goodbyeWorld", "applicationdb");
	if ($db_connection->connect_error) {
		die($db_connection->connect_error);
	} 

	// NEED TO ENCRYPT PASSWORD. POSSIBLY DONE IN LECTURE COMING WEEK?
	$query = "insert into applicants values(\"$name\", \"$email\", $gpa, $year, '$gender', \"$password\")";
	
	$result = $db_connection->query($query);
	if(!$result){
		die("Insertion failed: " . $db_connection->error);
	} else {
		echo "<h3>The following entry has been added to the database</h3>";
		echo "<b>Name:&nbsp;</b>";
		echo $name;
		echo "<br><b>Email:&nbsp;</b>";
		echo $email;
		echo "<br><b>Gpa:&nbsp;</b>";
		echo $gpa;
		echo "<br><b>Year:&nbsp;</b>";
		echo $year;
		echo "<br><b>Gender:&nbsp;</b>";
		echo $gender;
		echo <<<BODY
		<br><br>
		<form action="confirmation.php" method="post">
			<input type="submit" name="return" value="Return to main menu">
		</form>
BODY;
	}

	$db_connection->close();
?>

</body>
</html>