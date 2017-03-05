<!DOCTYPE html>
<html>
<head>
	<title>Display Applications</title>
</head>
<body>
	<h1>Applications</h1>
	<?php
		if(isset($_POST['return'])){
			header("Location: main.html");
		}

		$fields = $_POST['fields']; //should be an array
		$sort = $_POST['sort'];
		$condition = $_POST['condition'];

		//process fields into string
		$fieldsNew = "";
		for($i=0; $i<sizeof($fields); $i++){
			if($i!==0){$fieldsNew .= ", ";}
			$fieldsNew .= $fields[$i];
		}

		function displayTable($data, $fields){
			echo "<tr>";
			foreach ($fields as $key) {
				echo "<td>$data[$key]</td>";
			}
			echo "</tr>";
		}

		$db_connection = new mysqli("localhost", "dbuser", "goodbyeWorld", "applicationdb");
	if ($db_connection->connect_error) {
		die($db_connection->connect_error);
	}

	// NEED TO ENCRYPT PASSWORD. POSSIBLY DONE IN LECTURE COMING WEEK?
	$query = <<<QUERY
	SELECT $fieldsNew FROM applicants WHERE $condition ORDER BY $sort;
QUERY;
	// echo $query;
	
	$result = $db_connection->query($query);
	if(!$result){
		die("Retrieval failed: ". $db_connection->error);
	} else {
		$num_rows = $result->num_rows;
		if ($num_rows === 0) { //no results
			echo "No applications match your criteria";
		} else { //found results, time to process them
			echo "<table border=\"1\">";
			echo "<tr>";
			foreach ($fields as $key) {
				$heading = ucfirst($key);
				echo "<th>$heading</th>";
			}
			echo "</tr>";
			for ($row_index = 0; $row_index < $num_rows; $row_index++) {
				$result->data_seek($row_index);
				$row = $result->fetch_array(MYSQLI_ASSOC);
				displayTable($row, $fields);
			}
			echo "</table>";
		}
	}

	$result->close();

	$db_connection->close();

	?>
	<br>
	<form action="adminDisplay.php" method="post">	
		<input type="submit" name="return" value="Return to main menu">
	</form>

</body>
</html>