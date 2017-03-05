<!DOCTYPE html>
<html>
<head>
	<title>Review</title>
</head>
<body>
<?php
	session_start();
	if(isset($_POST['return'])){
		header("Location: main.html");
	}

	$email = $_SESSION['email'];
	$password = $_SESSION['password'];

	$db_connection = new mysqli("localhost", "dbuser", "goodbyeWorld", "applicationdb");
	if ($db_connection->connect_error) {
		die($db_connection->connect_error);
	}

	// NEED TO ENCRYPT PASSWORD. POSSIBLY DONE IN LECTURE COMING WEEK?
	$query = <<<QUERY
	SELECT name, email, gpa, year, gender FROM applicants WHERE email="$email" AND password="$password";
QUERY;
	
	$result = $db_connection->query($query);
	if(!$result){
		die("Retrieval failed: ". $db_connection->error);
	} else {
		if ( $result->num_rows === 0) { //no results
			$_SESSION['loginMessage'] = "Invalid email and/or password";
			header("Location: login.php");
		} else {	
			$result->data_seek(0);
			$data = $result->fetch_array(MYSQLI_ASSOC);
			
			echo "<h3>Application found in the database with the following values:</h3>";
			echo "<b>Name:&nbsp;</b>";
			echo $data['name'];
			echo "<br><b>Email:&nbsp;</b>";
			echo $data['email'];
			echo "<br><b>Gpa:&nbsp;</b>";
			echo $data['gpa'];
			echo "<br><b>Year:&nbsp;</b>";
			echo $data['year'];
			echo "<br><b>Gender:&nbsp;</b>";
			echo $data['gender'];
			echo <<<BODY
			<br><br>
			<form action="confirmation.php" method="post">
				<input type="submit" name="return" value="Return to main menu">
			</form>
BODY;
		}
	}
	
	/* Freeing memory */
	$result->close();

	$db_connection->close();
?>

</body>
</html>