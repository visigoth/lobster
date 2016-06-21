Vagrant.configure(2) do |config|
  config.vm.box = "fedora/23-cloud-base"

  config.vbguest.auto_update = true
  config.vbguest.auto_reboot = true
  config.vbguest.installer = VagrantVbguest::Installers::RedHat

  config.vm.provider "virtualbox" do |v|
    v.memory = 4096
    v.cpus = 2
  end

  config.vm.network :forwarded_port, guest: 8000, host: 8000, id: "v3spa"
  config.vm.provision "shell", path: "vagrant/provision.sh"
  config.vm.synced_folder ".", "/vagrant", type: "virtualbox"
end
