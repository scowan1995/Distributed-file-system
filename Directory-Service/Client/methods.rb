def post_file_add_by_name(excon,name)excon.request(:method=>:post,:path=>"/file/add/#{name}",:headers=>{},:body=>nil)end
def get_file_get_by_name(excon,name)excon.request(:method=>:get,:path=>"/file/get/#{name}",:headers=>{},:body=>nil)end
def post_addCluster_by_ip(excon,ip)excon.request(:method=>:post,:path=>"/addCluster/#{ip}",:headers=>{},:body=>nil)end
